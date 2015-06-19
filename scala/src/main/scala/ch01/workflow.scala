package ch01

import java.util.Date
import scala.collection.mutable.ListBuffer


case class Transition(from: Activity, to: Activity)


class Activity(val id: String, val activities: Set[Activity] = Set.empty, val transitions: Set[Transition] = Set.empty) {

  def copy(id: String = id, activities: Set[Activity] = activities, transitions: Set[Transition] = transitions): Activity = {
    new Activity(id, activities, transitions)
  }

  def +(transition: (String, String)): Activity = transition match { case(fromId, toId) =>
    (for {
      from <- activities.find(_.id == fromId)
      to <- activities.find(_.id == toId)
    } yield (from, to)).map { case (from, to) =>
      val fromWithTransition = from.copy(transitions = from.transitions + Transition(from, to))
      copy(activities = activities.filterNot(_.id == fromId) + fromWithTransition)
    }.getOrElse(this)
  }

  def start(): ActivityInstance = {
    val activityInstance = ActivityInstance(this)
    execute(activityInstance)
    activityInstance
  }

  def execute(activityInstance: ActivityInstance): Unit = {
    val toActivities = activities.flatMap(_.transitions).map(_.to)
    val startActivities = activities.toSet.diff(toActivities)
    startActivities.foreach(startActivity => activityInstance.execute(startActivity))
  }
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
    activity.transitions.foreach(transition => take(transition))
  }

  def take(transition: Transition): Unit = {
    endTime = Some(new Date())
    parent.map(_.execute(transition.to))
  }
}
