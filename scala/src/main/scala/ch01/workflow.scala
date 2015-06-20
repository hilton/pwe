package ch01

import java.util.Date
import scala.collection.mutable.ListBuffer


/** A process defined by activities (nodes) in the process, and their transitions. */
case class Workflow(activities: Set[Activity] = Set.empty) {

  /** Returns a copy of this workflow with the transition specified by the pair of activity IDs added.*/
  def +(transition: (String, String)): Workflow = transition match { case(fromId, toId) =>
    (for {
      from <- activities.find(_.id == fromId)
      to <- activities.find(_.id == toId)
    } yield (from, to)).map { case (from, to) =>
      val fromWithTransition = from.copy(transitions = from.transitions + Transition(from, to))
      copy(activities = activities.filterNot(_.id == fromId) + fromWithTransition)
    }.getOrElse(this)
  }

  /** Starts execution of this workflow, by executing activities with no incoming transition. */
  def start(): WorkflowInstance = {
    val workflowInstance = WorkflowInstance(this)
    val toActivities = activities.flatMap(_.transitions).map(_.to)
    val startActivities = activities.toSet.diff(toActivities)
    startActivities.foreach(startActivity => workflowInstance.execute(startActivity))
    workflowInstance
  }
}


/** A sequence flow from one workflow activity to another. */
case class Transition(from: Activity, to: Activity)


/** A workflow activity (a.k.a. task) that has transitions to other activities. */
abstract class Activity(val id: String, val transitions: Set[Transition] = Set.empty) {

  def copy(id: String = id, transitions: Set[Transition] = transitions): Activity

  def execute(activityInstance: ActivityInstance): Unit
}


/** A single case of running a workflow. */
case class WorkflowInstance(workflow: Workflow, activityInstances: ListBuffer[ActivityInstance] = new ListBuffer()) {
  
  /** Executes an activity within this workflow instance. */
  def execute(activity: Activity): Unit = {
    val activityInstance = ActivityInstance(activity, this)
    activityInstances.append(activityInstance)
    activity.execute(activityInstance)
  }
}


/** A single case of running an activity. */
case class ActivityInstance(activity: Activity, workflowInstance: WorkflowInstance, var endTime: Option[Date] = None) {

  val startTime = new Date()

  /** Completes this execution and takes transitions to the next activities. */
  def end(): Unit = {
    endTime = Some(new Date())
    activity.transitions.foreach(transition => workflowInstance.execute(transition.to))
  }
}
