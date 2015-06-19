package ch01

import java.util.Date
import scala.collection.mutable.ListBuffer


class Activity(val id: String, val activities: Set[Activity] = Set.empty, val transitions: Set[Activity] = Set.empty) {

  def copy(id: String = id, activities: Set[Activity] = activities, transitions: Set[Activity] = transitions): Activity = {
    new Activity(id, activities, transitions)
  }

  def +(transition: (String, String)): Activity = transition match { case(fromId, toId) =>
    activities.find(_.id == fromId).map { from =>
      val to = activities.filter(_.id == toId)
      val updatedFrom = from.copy(transitions = from.transitions ++ to)
      copy(activities = activities.filterNot(_.id == fromId) + updatedFrom)
    }.getOrElse(this)
  }

  def start(): ActivityInstance = {
    val activityInstance = ActivityInstance(this)
    execute(activityInstance)
    activityInstance
  }

  def execute(activityInstance: ActivityInstance): Unit = {
    val toActivities = activities.flatMap(_.transitions)
    val startActivities = activities.toSet.diff(toActivities)
    startActivities.foreach(startActivity => activityInstance.execute(startActivity))
  }

  override def toString = s"Auto[$id, $activities]" + transitions.map("→" + _.id).mkString("")
}


case class ActivityInstance(activity: Activity, parent: Option[ActivityInstance] = None,
  var endTime: Option[Date] = None, activityInstances: ListBuffer[ActivityInstance] = new ListBuffer()) {

  val startTime = new Date()

  def execute(activity: Activity): Unit = {
    val activityInstance = ActivityInstance(activity, Some(this))
    activityInstances.append(activityInstance)
    activity.execute(activityInstance)
  }

  def end(): Unit = {
    endTime = Some(new Date())
    activity.transitions.foreach(takeTransitionTo(_))
  }

  def takeTransitionTo(activity: Activity): Unit = {
    endTime = Some(new Date())
    parent.map(_.execute(activity))
  }
}
