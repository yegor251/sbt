import zio._
import consoleAdmin.CommandExecutor
import consoleAdmin._

object ConsoleAdmin {
	val functionCallRegex = """([a-z]+)\s*\((.*)\)""".r
	val paramRegex = """"[^"]*"|\d+[lL]|\d+""".r

	def handleInput(inputString: String): ZIO[Any, Throwable, Unit] = {
		inputString match {
			case functionCallRegex(name, params) => ZIO.succeed(CommandExecutor.run(name, paramRegex.findAllIn(params).toList.map {
				case s if s.matches("""".*"""") => s.drop(1).dropRight(1)
				case l if l.matches("""\d+[lL]""") => l.dropRight(1).toLong
				case i if i.matches("""\d+""") => i.toInt
			}))
			case "stop"   => ZIO.succeed(println("Stopping server..."))
			case _      	=> ZIO.succeed(println(s"Not a function: $inputString"))
		}
	}

	// Command listener that listens continuously and stops on "stop"
	val commandListener: ZIO[Any, Throwable, Unit] = 
		ZIO
			.attempt(scala.io.StdIn.readLine()) // Read input
			.flatMap { input =>
				handleInput(input) *>
					ZIO.when(input == "stop")(ZIO.fail(new Exception("Stop command received")))
			}
			.forever.catchAll(_ => ZIO.unit) // Stop the loop if "stop" is entered
}