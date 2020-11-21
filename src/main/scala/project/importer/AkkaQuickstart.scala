//#full-example
package project.importer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.{Automata, MessageBundle, MyTransition, SecureActor}
import akka.actor.Props
import project.importer.A.{SetAutoa, SetBa, SetCa, a0, a1, a2, a3}
import project.importer.B.{SetAb, SetAutob, SetCb, b0, b1, b2}
import project.importer.C.{SetAc, SetAutoc, SetBc, c0, c1, c2}

//import com.example.
//#greeter-actor

object A {
  def props(): Props = Props(new A())
  case class a0()
  case class a1()
  case class a2()
  case class a3()
  case class SetBa(b: ActorRef)
  case class SetCa(c: ActorRef)
  case class SetAutoa(aut: Automata)
}

object C {
  def props(): Props = Props(new C())
  case class c0()
  case class c1()
  case class c2()
  case class SetAc(a: ActorRef)
  case class SetBc(b: ActorRef)
  case class SetAutoc(aut: Automata)
}

object B {
  def props(): Props = Props(new B())
  case class b0()
  case class b1()
  case class b2()
  case class SetAb(a: ActorRef)
  case class SetCb(c: ActorRef)
  case class SetAutob(aut: Automata)
}

class A extends SecureActor{
    import A._
    var j: Boolean = false
    var f: Boolean = true
    var c: Int = 4
    var B: ActorRef = null
    var C: ActorRef = null
    var automata: Automata = null
    def receive = manageControls.orElse(manageNormals).orElse(manageMessages)
    val manageMessages: Receive= {
      case SetAutoa(aut)=>
        automata = aut

      case SetBa(b) => {
        B = b
      }

      case SetCa(c) => {
        C = c
      }

      case a0() => {
        println("Normal Message a0 " + " " + self.path.name)
        f = false
        sendSecureMessage(C, c2(),automata)
        if (f)
          c = 4
        c = 7
        sendSecureMessage(B, b0(),automata)
        sendSecureMessage(B, b1(),automata)
      }

      case a1() => {
        println("Normal Message a1 " + " " + self.path.name)
        j = false
        if(j)
          f = true
        B ! b1()
        if(j)
          c = 4
        sendSecureMessage(B, b0(),automata)
      }

      case a2() => {
        println("Normal Message a2 " + " " + self.path.name)
        c = 6
        if(f)
          j = false
        C ! c2()
        if(j)
          c = 4
        sendSecureMessage(C, c1(),automata)
        sendSecureMessage(B, b1(),automata)
        sendSecureMessage(B, b0(),automata)
      }

      case a3() => {
        println("Normal Message a3 " + " " + self.path.name)
        if(f)
          c = 1
        sendSecureMessage(B, b1(),automata)
      }
    }

}

class B extends SecureActor{
  import B._
  var e: Boolean = false
  var l: Boolean = false
  var A: ActorRef = null
  var C: ActorRef = null
  var automata: Automata = null
  def receive = manageControls.orElse(manageNormals).orElse(manageMessages)
  val manageMessages: Receive = {
    case SetAutob(aut) =>
      automata = aut

    case SetAb(a) =>
      A = a

    case SetCb(c) =>
      C = c

    case b0() => {
      println("Normal Message b0 " + " " + self.path.name)
      l = false
      sendSecureMessage(A, a3(),automata)
    }

    case b1() => {
      println("Normal Message b1 " + " " + self.path.name)
      e = false
    }

    case b2() => {
      println("Normal Message b2 " + " " + self.path.name)
      if(l)
        e = false
      sendSecureMessage(A, a2(),automata)
      if(e)
        l = false
      sendSecureMessage(A, a0(),automata)
      sendSecureMessage(A, c1(),automata)
      sendSecureMessage(A, a3(),automata)
      sendSecureMessage(A, a1(),automata)
      sendSecureMessage(C, c2(),automata)
    }
  }

}

class C extends SecureActor{
  import C._
  var f: Boolean = true
  var a: Boolean = true
  var A: ActorRef = null
  var B: ActorRef = null
  var automata: Automata = null
  //can cause problem
  //self ! c0()
  def receive = manageControls.orElse(manageNormals).orElse(manageMessages)
  val manageMessages: Receive = {
    case SetAutoc(aut) =>{
      automata = aut
    }
    case SetAc(a) => {
      A = a
    }

    case SetBc(b) => {
      B = b
    }

    case c0() => {
      println("Normal Message c0 " + " " + self.path.name)
      a = true
      f = false
      sendSecureMessage(B, b0(),automata)
      sendSecureMessage(A, a2(),automata)
      sendSecureMessage(A, a1(),automata)
      sendSecureMessage(A, a3(),automata)
      sendSecureMessage(A, a0(),automata)
      sendSecureMessage(B, b2(),automata)
      sendSecureMessage(B, b1(),automata)
    }

    case c1() => {
      println("Normal Message c0 " + " " + self.path.name)
      f = false
      A ! a0()
      a = false
      A ! a3()
      if(f)
        a = true
      sendSecureMessage(B, b1(),automata)
      sendSecureMessage(A, a1(),automata)
      sendSecureMessage(B, b0(),automata)


    }
    case c2()=>
    {
      println("Normal Message c2 " + " " + self.path.name)
      a = false
      sendSecureMessage(B, b0(),automata)
      sendSecureMessage(B, a1(),automata)
      f = true
      sendSecureMessage(B, b1(),automata)
      sendSecureMessage(A, a3(),automata)
    }
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
    val a_actor: ActorRef =
      system.actorOf(A.props().withDispatcher("custom-dispatcher"),"A_Actor")
    val b_actor: ActorRef =
      system.actorOf(B.props().withDispatcher("custom-dispatcher"),"B_Actor")
    val c_actor: ActorRef =
      system.actorOf(C.props().withDispatcher("custom-dispatcher"),"C_Actor")

    val customAutomata: Automata = new Automata
    val bndl1: MessageBundle = new MessageBundle(a_actor, c1(), c_actor)
    val bndl2: MessageBundle = new MessageBundle(c_actor, a2(), a_actor)
    val bndl3: MessageBundle = new MessageBundle(b_actor, c1(), c_actor)
    val bndl4: MessageBundle = new MessageBundle(c_actor, a3(), a_actor)
    val bndl5: MessageBundle = new MessageBundle(a_actor, b1(), b_actor)
    val bndl6: MessageBundle = new MessageBundle(c_actor, a0(), a_actor)
    val bndl7: MessageBundle = new MessageBundle(c_actor, a1(), a_actor)
    val trn1: MyTransition = new MyTransition(0,1, bndl1 ,true)
    val trn2: MyTransition = new MyTransition(0,4, bndl2 ,true)
    val trn3: MyTransition = new MyTransition(0,5, bndl3 ,true)
    val trn4: MyTransition = new MyTransition(1,2, bndl4 ,true)
    val trn5: MyTransition = new MyTransition(2,3, bndl5 ,true)
    val trn6: MyTransition = new MyTransition(4,2, bndl6 ,true)
    val trn7: MyTransition = new MyTransition(5,2, bndl7 ,true)
    customAutomata.addTransition(trn1)
    customAutomata.addTransition(trn2)
    customAutomata.addTransition(trn3)
    customAutomata.addTransition(trn4)
    customAutomata.addTransition(trn5)
    customAutomata.addTransition(trn6)
    customAutomata.addTransition(trn7)
    customAutomata.addLastTransition(3)
    a_actor ! SetBa(b_actor)
    a_actor ! SetCa(c_actor)
    a_actor ! SetAutoa(customAutomata)
    b_actor ! SetAb(a_actor)
    b_actor ! SetCb(c_actor)
    b_actor ! SetAutob(customAutomata)
    c_actor ! SetAc(a_actor)
    c_actor ! SetBc(b_actor)
    c_actor ! SetAutoc(customAutomata)
    c_actor ! SendOrderMessage(c_actor, c0(), customAutomata)
  //#main-send-messages
  //#main-send-messages
}
//#main-class
//#full-example
