package ch01

import org.scalatest._

case class Automatic(override val id: String) extends Activity(id) {
  override def execute(activityInstance: ActivityInstance): Unit = {
    println(s"Executing $id")
    activityInstance.finish()
  }
}

case class Wait(override val id: String) extends Activity(id) {
  override def execute(activityInstance: ActivityInstance): Unit = println(s"Executing $id")
}

class ExecutionSpec extends FlatSpec with Matchers {

  "A workflow" should "do sequential automatic execution" in {
    val workflow = Activity("workflow") + Automatic("a") + Automatic("b") + ("a" -> "b")
    val workflowInstance = workflow.start()
    val openActivities = workflowInstance.activityInstances.filter(_.end.isEmpty)
    assert(openActivities.isEmpty)
    assert(workflowInstance.activityInstances.filter(_.activity.id == "a").forall(_.end.isDefined))
    assert(workflowInstance.activityInstances.filter(_.activity.id == "b").forall(_.end.isDefined))
  }

  it should "do parallel automatic execution" in {
    val workflow = Activity("workflow") + Automatic("a") + Automatic("b") + ("a" -> "b")
    val workflowInstance = workflow.start()
    val openActivities = workflowInstance.activityInstances.filter(_.end.isEmpty)
    assert(openActivities.isEmpty)
    assert(workflowInstance.activityInstances.filter(_.activity.id == "a").forall(_.end.isDefined))
    assert(workflowInstance.activityInstances.filter(_.activity.id == "b").forall(_.end.isDefined))
  }
}
