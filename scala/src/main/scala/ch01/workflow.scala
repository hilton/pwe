package ch01

import java.util.Date
import scala.collection.mutable.ListBuffer

case class Transition(from: Activity, to: Activity)


class Activity(val id: String, val activities: Seq[Activity] = Seq.empty, val transitions: Seq[Transition] = Seq.empty) {

  def +(activity: Activity): Activity = Activity(id, activities :+ activity, transitions)

  def +(fromTo: Tuple2[String, String]): Activity = {
    val from = activities.find(_.id == fromTo._1).get
    val to = activities.find(_.id == fromTo._2).get
    Activity(id, activities, transitions :+ Transition(from, to))
  }

  def start(): ActivityInstance = {
    val activityInstance = ActivityInstance(this)
    execute(activityInstance)
    activityInstance
  }

  def execute(activityInstance: ActivityInstance): Unit = {
    val toActivities = activities.flatMap(_.transitions).map(_.to).toSet
    val startActivities = activities.toSet.diff(toActivities)
    startActivities.foreach(startActivity => activityInstance.execute(startActivity))
  }
}

object Activity {
  def apply(id: String, activities: Seq[Activity] = Seq.empty, transitions: Seq[Transition] = Seq.empty) = {
    new Activity(id, activities, transitions)
  }
}


case class ActivityInstance(activity: Activity, parent: Option[ActivityInstance] = None, start: Option[Date] = None,
  var end: Option[Date] = None, activityInstances: ListBuffer[ActivityInstance] = new ListBuffer()) {

  def execute(activity: Activity): Unit = {
    val activityInstance = ActivityInstance(activity, Some(this))
    activityInstances.append(activityInstance)
    activity.execute(activityInstance)
  }

  def finish(): Unit = {
    end = Some(new Date())
    activity.transitions.foreach(take(_))
  }

  def take(transition: Transition): Unit = {
    end = Some(new Date())
    parent.map(_.execute(transition.to))
  }
}