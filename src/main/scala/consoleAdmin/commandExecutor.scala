package consoleAdmin

import java.lang.reflect.Method
import scala.reflect
import scala.reflect.ClassTag
import javax.xml.namespace.QName
import Console.{RESET, RED, BOLD, GREEN, WHITE}

object CommandExecutor {
	val commandsInst = new Commands();

	def getMethodArguments[T](using ct: ClassTag[T]): Seq[(String, Seq[Class[_]])] = {
		val clazz = ct.runtimeClass
		clazz.getMethods.map { method =>
			(method.getName, method.getParameterTypes.toSeq)
		}.toSeq
	}

	def invokeMethod(instance: Any, methodName: String, args: Seq[Any]) = {
		val clazz = instance.getClass
		val methodOpt = clazz.getMethods.find(_.getName == methodName)

		methodOpt match
			case None => println(s"${RED}Invalid method $methodName${RESET}")
			case Some(method) => 
				try 
					println(method.invoke(instance, args: _*))
				catch
					case e: Throwable => println(s"${RED}Error occured while invoking method $methodName(${args.mkString(", ")}). Error message: ${e.getMessage()}${RESET}")
					case _ => println(s"${RED}Could not invoke method $methodName(${args.mkString(", ")})${RESET}")
	}

	def run(name: String, args: Seq[Any]) = invokeMethod(commandsInst, name, args)
}
