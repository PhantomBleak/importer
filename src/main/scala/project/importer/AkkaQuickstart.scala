//#full-example
package project.importer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.{Automata, MessageBundle, MyTransition, SecureActor}
import akka.actor.Props
import com.SecureActor.{NormalMessage, StashedNormalMessage}
import project.importer.SecondHandActor.NormalMessageTypeA
//import com.example.
//#greeter-actor

object SecondHandActor {
  def props(): Props = Props(new SecondHandActor())
  case class NormalMessageTypeA(message: String)
}

class SecondHandActor extends SecureActor{
    import SecondHandActor._
    def receive = manageControls.orElse(manageNormals).orElse(manageMessages)
    val manageMessages: Receive= {
      case NormalMessageTypeA(message) =>
        println("Normal Message TypeA " + message + " " + self.path.name)

    }

}
//#greeter-actor

//#greeter-bot

//#greeter-main

//#main-class
object AkkaQuickstart extends App {
  //#actor-system
  //#actor-system
    import SecureActor._
    val system: ActorSystem = ActorSystem("helloAkka")
    val firstActor: ActorRef =
      system.actorOf(SecondHandActor.props().withDispatcher("custom-dispatcher"),"firstActor")
    val secondActor: ActorRef =
      system.actorOf(SecondHandActor.props().withDispatcher("custom-dispatcher"),"secondActor")
    val customAutomata: Automata = new Automata
    val customBundle: MessageBundle = new MessageBundle(secondActor, NormalMessage(NormalMessageTypeA("a0")), firstActor)
    val customTransition: MyTransition = new MyTransition(0,1, customBundle ,true)
    val customBundle2: MessageBundle = new MessageBundle(firstActor, NormalMessage(NormalMessageTypeA("b1")),secondActor)
    val customTransition2: MyTransition = MyTransition(1, 2, customBundle2, true)
    customAutomata.addTransition(customTransition)
    customAutomata.addTransition(customTransition2)
    customAutomata.addLastTransition(2)
    secondActor ! SendOrderMessage(firstActor, NormalMessage(NormalMessageTypeA("a0")), customAutomata)
    firstActor ! SendOrderMessage(secondActor, NormalMessage(NormalMessageTypeA("b1")), customAutomata)
  //#main-send-messages
  //#main-send-messages
}
//#main-class
//#full-example
