package ch01

import java.util.Date
import scala.collection.mutable.ListBuffer


/** A sequence flow from one workflow activity to another. */
case class Transition(from: Activity, to: Activity)


/**
 * A workflow activity (a.k.a. task) that may have nested activities and transitions to other activities. A workflow is
 * just a top-level activity with no transitions. Custom activity types are subclasses that override execute.
 */
class Activity(val id: String, val activities: Set[Activity] = Set.empty, val transitions: Set[Transition] = Set.empty) {

  protected def copy(id: String = id, activities: Set[Activity] = activities, transitions: Set[Transition] = transitions): Activity = {
    new Activity(id, activities, transitions)
  }

  /** Returns a copy of this activity with the transition specified by the pair of activity IDs added.*/
  def +(transition: (String, String)): Activity = transition match { case(fromId, toId) =>
    (for {
      from <- activities.find(_.id == fromId)
      to <- activities.find(_.id == toId)
    } yield (from, to)).map { case (from, to) =>
      val fromWithTransition = from.copy(transitions = from.transitions + Transition(from, to))
      copy(activities = activities.filterNot(_.id == fromId) + fromWithTransition)
    }.getOrElse(this)
  }

  /** Starts execution of this activity. */
  def start(): ActivityInstance = {
    val activityInstance = ActivityInstance(this)
    execute(activityInstance)
    activityInstance
  }

  /** Executes this activities nested activities, in the context of the given activity instance. */
  def execute(activityInstance: ActivityInstance): Unit = {
    val toActivities = activities.flatMap(_.transitions).map(_.to)
    val startActivities = activities.toSet.diff(toActivities)
    startActivities.foreach(startActivity => activityInstance.execute(startActivity))
  }
}


/** An execution of an activity, which is an execution scope, with an optional parent execution. */
case class ActivityInstance(activity: Activity, parent: Option[ActivityInstance] = None,
  var endTime: Option[Date] = None, activityInstances: ListBuffer[ActivityInstance] = new ListBuffer()) {

  val startTime = new Date()

  /** Executes an activity within this execution scope. */
  def execute(activity: Activity): Unit = {
    val activityInstance = ActivityInstance(activity, Some(this))
    activityInstances.append(activityInstance)
    activity.execute(activityInstance)
  }

  /** Completes this execution and takes transitions to the next activities. */
  def end(): Unit = {
    endTime = Some(new Date())
    activity.transitions.foreach(transition => take(transition))
  }

  /** Takes a transition to (executes) another activity. */
  def take(transition: Transition): Unit = {
    endTime = Some(new Date())
    parent.map(_.execute(transition.to))
  }
}
