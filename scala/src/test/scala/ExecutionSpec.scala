package ch01

import org.scalatest._

/** An activity that completes automatically, with user intervention, like an automated service task. */
case class Automatic(override val id: String, override val transitions: Set[Transition] = Set.empty)
  extends Activity(id, transitions) {

  override def copy(id: String = id, transitions: Set[Transition] = transitions): Activity = {
    Automatic(id, transitions)
  }

  override def execute(activityInstance: ActivityInstance): WorkflowInstance = {
    println(s"Automatic: executing activity $id")
    activityInstance.end()
  }
}

/**
 * An activity that never completes, as if waiting for an external signal (not implemented), such as user interaction.
 */
case class Wait(override val id: String, override val transitions: Set[Transition] = Set.empty)
  extends Activity(id, transitions) {

  override def copy(id: String = id, transitions: Set[Transition] = transitions): Activity = {
    Wait(id, transitions)
  }

  override def execute(activityInstance: ActivityInstance): WorkflowInstance = {
    println(s"Wait: executing activity $id")
    activityInstance.workflowInstance
  }
}

class ExecutionSpec extends FlatSpec with Matchers {

  "A workflow" should "do sequential automatic execution" in {
    val workflow = new Workflow(Set(Automatic("a"), Automatic("b"))) + ("a" -> "b")
    val workflowInstance = WorkflowInstance(workflow).start()
    assert(openActivities(workflowInstance).isEmpty, "open")
    assert(completedActivities(workflowInstance) == Set("a", "b"), "complete")
  }

  it should "do parallel automatic execution" in {
    val workflow = new Workflow(Set(Automatic("a"), Automatic("b")))
    val workflowInstance = WorkflowInstance(workflow).start()
    assert(openActivities(workflowInstance).isEmpty, "open")
    assert(completedActivities(workflowInstance) == Set("a", "b"), "complete")
  }

  it should "do sequential wait execution" in {
    val workflow = new Workflow(Set(Wait("a"), Wait("b"))) + ("a" -> "b")
    val workflowInstance = WorkflowInstance(workflow).start()
    assert(openActivities(workflowInstance) == Set("a"), "open")
    assert(completedActivities(workflowInstance).isEmpty, "complete")
  }

  it should "do parallel wait execution" in {
    val workflow = new Workflow(Set(Wait("a"), Wait("b")))
    val workflowInstance = WorkflowInstance(workflow).start()
    assert(openActivities(workflowInstance) == Set("a", "b"), "open")
    assert(completedActivities(workflowInstance).isEmpty, "complete")
  }

  private def openActivities(workflowInstance: WorkflowInstance): Set[String] = {
    workflowInstance.activityInstances.filter(_.endTime.isEmpty).map(_.activity.id).toSet
  }

  private def completedActivities(workflowInstance: WorkflowInstance): Set[String] = {
    workflowInstance.activityInstances.filter(_.endTime.isDefined).map(_.activity.id).toSet
  }
}
