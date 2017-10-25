package chip8
package assembler

import scala.util.parsing.combinator._

import model.{InstructionSet => Is}


object Assembly {
  sealed trait Operand
  case class V(index: Short) 
    extends Operand
  case class Immediate(value: Short)
    extends Operand
  case class Special(name: String)
    extends Operand

  sealed trait Target
    extends Operand
  case class Label(name: String)
    extends Target
  case class Address(value: Short)
    extends Target

  sealed trait Instruction
  case class InstructionText(mnemonic: String, 
                             operands: Seq[Operand])
    extends Instruction
  case class TranslatedInto(it: Is.Instruction)
    extends Instruction
  case class ErroneousTranslation(text: InstructionText, message: String)
    extends Instruction
  case class Remark(text: String)
  case class Line(label: Option[String], 
            instruction: Instruction,
                 remark: Option[Remark])

  case class ProgramText(lines: Seq[Line]) {
    def map(f: Line => Line): ProgramText =
      copy(lines = lines map f)
    def mapLines(f: Seq[Line] => Seq[Line]) =
      copy(f(lines))
  }

  def computeLabelAddresses(start: Int, 
                             lines: Seq[Line]): Map[String, Short] = 
    lines.zipWithIndex.collect {
      case (Line(Some(target), _, _), i) =>
        target -> (start + i * 2).asInstanceOf[Short]
    }.toMap

  def rewriteJumpTargets(lines: Seq[Line], 
                     targetMap: Map[String, Short]): Seq[Line] =
    lines map {
      case ln @ Line(_, i @ InstructionText(mn, Seq(Label(target))), _) 
          if mn.toUpperCase == "JP" =>
        // Report error when `target` is not in `targetMap`
        ln.copy(instruction = i.copy(operands = Seq(Address(targetMap(target)))))
      case ln => ln
    }

  def emit(n3: Int, n2: Int, n1: Int, n0: Int) =
    n3 << 3 * 4 | n2 << 2 * 4 | n1 << 1 * 4 | n0 << 0 * 4 & 0xFFFF
  
  def emit(n3: Int, n2: Int, byte: Short) =
    n3 << 3 * 4 | n2 << 2 * 4 | byte

  def emit(n3: Int, tripple: Int) =
    n3 << 3 * 4 | tripple

  def encodeOpCode(instruction: Is.Instruction): Int = instruction match {
    case Is.Cls                                          => 0x00E0
    case Is.Ret                                          => 0x00EE
    case Is.Sys(address)                                 => emit(0x0, address)
    case Is.Jp(address)                                  => emit(0x1, address)
    case Is.JpV0(address)                                => emit(0xB, address)
    case Is.Call(address)                                => emit(0x2, address)
    case Is.Se(x, Is.Immediate(byte))                    => emit(0x3, x, byte)
    case Is.Se(x, Is.Register(y))                        => emit(0x5, x, y, 0x0)
    case Is.Sne(x, Is.Immediate(byte))                   => emit(0x4, x, byte)
    case Is.Sne(x, Is.Register(y))                       => emit(0x9, x, y, 0x0)
    case Is.Ld(Is.Register(x), Is.Immediate(byte))       => emit(0x6, x, byte)
    case Is.Ld(Is.Register(x), Is.Register(y))           => emit(0x8, x, y, 0x0)
    case Is.Ld(Is.Register(x), Is.DelayTimer)            => emit(0xF, x, 0x07)
    case Is.Ld(Is.Register(x), Is.WaitForKeyboard)       => emit(0xF, x, 0x0A)
    case Is.Ld(Is.Index, Is.Immediate(address))          => emit(0xA, address)
    case Is.Ld(Is.DelayTimer, Is.Register(x))            => emit(0xF, x, 0x15)
    case Is.Ld(Is.SoundTimer, Is.Register(x))            => emit(0xF, x, 0x18)
    case Is.Ld(Is.Index, Is.DigitSprite(Is.Register(x))) => emit(0xF, x, 0x29)
    case Is.LdBcd(Is.Register(x))                        => emit(0xF, x, 0x33)
    case Is.Add(Is.Register(x), Is.Immediate(byte))      => emit(0x7, x, byte)
    case Is.Add(Is.Register(x), Is.Register(y))          => emit(0x8, x, y, 0x4)
    case Is.Add(Is.Index, Is.Register(x))                => emit(0xF, x, 0x1E)
    case Is.Or(x, y)                                     => emit(0x8, x, y, 0x1)
    case Is.And(x, y)                                    => emit(0x8, x, y, 0x2)
    case Is.Xor(x, y)                                    => emit(0x8, x, y, 0x3)
    case Is.Sub(x, y)                                    => emit(0x8, x, y, 0x5)
    case Is.Subn(x, y)                                   => emit(0x8, x, y, 0x7)
    case Is.Shr(x, y)                                    => emit(0x8, x, y, 0x6)
    case Is.Shl(x, y)                                    => emit(0x8, x, y, 0xE)
    case Is.Rnd(x, byte)                                 => emit(0xC, x, byte)
    case Is.Drw(x,y, n)                                  => emit(0xD, x, y, n)
    case Is.Skp(x)                                       => emit(0xE, x, 0x9E)
    case Is.Sknp(x)                                      => emit(0xE, x, 0xA1)
    case Is.SaveRegs(x)                                  => emit(0xF, x, 0x55)
    case Is.RestoreRegs(x)                               => emit(0xF, x, 0x65)    
    case i => ???
  }

  def wrap(i: Is.Instruction) =
    TranslatedInto(i)

  def translate(text: InstructionText): Instruction = text match {
    case InstructionText("CLS", Nil) => 
      wrap(Is.Cls)
    case InstructionText("RET", Nil) => 
      wrap(Is.Ret)
    case InstructionText("SYS", Immediate(address) :: Nil) => 
      wrap(Is.Sys(address))
    case InstructionText("JP", Address(address) :: Nil) => 
      wrap(Is.Jp(address))
    case InstructionText("JP", V(0) :: Immediate(address) :: Nil) => 
      wrap(Is.JpV0(address))
    case InstructionText("CALL", Immediate(address) :: Nil) => 
      wrap(Is.Call(address))
    case InstructionText("SE", V(x) :: Immediate(byte) :: Nil) => 
      wrap(Is.Se(x, Is.Immediate(byte)))
    case InstructionText("SE", V(x) :: V(y) :: Nil) => 
      wrap(Is.Se(x, Is.Register(y)))
    case InstructionText("SNE", V(x) :: Immediate(byte) :: Nil) => 
      wrap(Is.Se(x, Is.Immediate(byte)))
    case InstructionText("SNE", V(x) :: V(y) :: Nil) => 
      wrap(Is.Se(x, Is.Register(y)))
    case InstructionText("LD", V(x) :: Immediate(byte) :: Nil) =>
      wrap(Is.Ld(Is.Register(x), Is.Immediate(byte)))
    case InstructionText("LD", V(x) :: V(y) :: Nil) =>
      wrap(Is.Ld(Is.Register(x), Is.Register(y)))
    case InstructionText("LD", Special("I") :: Immediate(byte) :: Nil) =>
      wrap(Is.Ld(Is.Index, Is.Immediate(byte)))
    case InstructionText("LD", V(x) :: Special("DT") :: Nil) =>
      wrap(Is.Ld(Is.Register(x), Is.DelayTimer))
    case InstructionText("LD", Special("DT") :: V(x) :: Nil) =>
      wrap(Is.Ld(Is.DelayTimer, Is.Register(x)))
    case InstructionText("LD", Special("ST") :: V(x) :: Nil) =>
      wrap(Is.Ld(Is.SoundTimer, Is.Register(x)))
    case InstructionText("LD", V(x) :: Special("K") :: Nil) =>
      wrap(Is.Ld(Is.Register(x), Is.WaitForKeyboard))
    case InstructionText("LD", Special("F") :: V(x) :: Nil) =>
      wrap(Is.Ld(Is.Index, Is.DigitSprite(Is.Register(x))))
    case InstructionText("LD", Special("B") :: V(x) :: Nil) =>
      wrap(Is.LdBcd(Is.Register(x)))
    case InstructionText("LD", Special("[I]") :: V(x) :: Nil) =>
      wrap(Is.SaveRegs(x))
    case InstructionText("LD",  V(x) :: Special("[I]") :: Nil) =>
      wrap(Is.RestoreRegs(x))
    case InstructionText("ADD", V(x) :: Immediate(byte) :: Nil) =>
      wrap(Is.Add(Is.Register(x), Is.Immediate(byte)))
    case InstructionText("ADD", V(x) :: V(y) :: Nil) =>
      wrap(Is.Add(Is.Register(x), Is.Register(y)))
    case InstructionText("ADD", V(x) :: Nil) =>
      wrap(Is.Add(Is.Index, Is.Register(x)))
    case InstructionText("OR", V(x) :: V(y) :: Nil) =>
      wrap(Is.Or(x, y))
    case InstructionText("AND", V(x) :: V(y) :: Nil) =>
      wrap(Is.And(x, y))
    case InstructionText("XOR", V(x) :: V(y) :: Nil) =>
      wrap(Is.Xor(x, y))
    case InstructionText("SUB", V(x) :: V(y) :: Nil) =>
      wrap(Is.Sub(x, y))
    case InstructionText("SUBN", V(x) :: V(y) :: Nil) =>
      wrap(Is.Subn(x, y))
    case InstructionText("SHR", V(x) :: Nil) =>
      wrap(Is.Shr(x, 0))
    case InstructionText("SHL", V(x) :: Nil) =>
      wrap(Is.Shl(x, 0))
    case InstructionText("RND", V(x) :: Immediate(byte) :: Nil) =>
      wrap(Is.Rnd(x, byte))
    case InstructionText("DRW", V(x) :: V(y) :: Immediate(nibble) :: Nil) =>
      wrap(Is.Drw(x, y, nibble))
    case InstructionText("SKP", V(x) :: Nil) =>
      wrap(Is.Skp(x))
    case InstructionText("SKNP", V(x) :: Nil) =>
      wrap(Is.Sknp(x))
    case faultyInstruction =>
      ErroneousTranslation(faultyInstruction,
                           "Unknown instruction or combination of instruction and operands.")
  }

  def translateProgram(p: ProgramText): ProgramText = p map {
    case line @ Line(_, instruction @ InstructionText(_, _), _) => 
      line.copy(instruction = translate(instruction))
    case line => line
  }
}

object Parser extends RegexParsers {
  def atom      = "[a-zA-Z0-9_]+".r
  def label     = atom <~ ":"

  def nibble    = "[0-9A-F]".r                            ^^ { t => Integer.parseInt(t, 16).asInstanceOf[Short] }
  def byte      = nibble ~ nibble                         ^^ { case t ~ u => ((t << 4) + u).asInstanceOf[Short] }
  def tripple   = nibble ~ byte                           ^^ { case t ~ u => ((t << 8) + u).asInstanceOf[Short] }

  def register  = ("v" | "V") ~> nibble                   ^^ Assembly.V
  def immediate = "#" ~> (tripple | byte | nibble)        ^^ Assembly.Immediate
  def special   = ("I" | "DT" | "ST" | "F" | "B" | "K")   ^^ Assembly.Special
  def target    = atom                                    ^^ Assembly.Label
  def operand   = register | immediate | special | target
  def remark    = ";" ~> "[^\\n]*".r                      ^^ Assembly.Remark

  def programText   = rep1(line)                          ^^ Assembly.ProgramText

  def line = label.? ~ atom ~ rep1sep(operand, ",") ~ remark.? ^^ {
    case label ~ mnemonic ~ operands ~ remark =>
      Assembly.Line(label, Assembly.InstructionText(mnemonic, operands), remark)
  }
}

object AsmPars extends App {
  import Parser._,
         Assembly._

  val source = """
    LD	V0, #10
    LD	V1, #2
    LD	V2, #1
  Loop:
    SUB	V0, V2
    ADD V1, #2
    SE	V0, #0
    JP	Loop
    HLT
  """

  implicit class PipeOps[A](subject: A) {
    def |> [B] (f: A => B): B = f(subject)
  }

  def translateProgramText(text: ProgramText) = 
    (text mapLines { lines =>
      rewriteJumpTargets(lines, computeLabelAddresses(0x200, lines))
    }) |> translateProgram

  def emitOutcome(program: ProgramText): String Either Seq[(Is.Instruction, Int)] = {
    val errors = program.lines collect {
      case Line(_, ErroneousTranslation(ins, becauseReason), _) =>
        val instruction = s"$ins.mnemonic ${ins.operands mkString ","}"
        s"ERROR: `$instruction`: $becauseReason"
    }

    if (errors.nonEmpty) Left(errors mkString "\n")
    else Right(program.lines map {
      case Line(_, TranslatedInto(instruction), _) =>
        instruction -> encodeOpCode(instruction)
    })
  }

  def reportLine(line: (Is.Instruction, Int)): Unit = {
    println(s"${Integer.toString(line._2, 16)} -- ${line._1}")
  }

  parse(programText, source) match {
    case Success(program, _) =>
      (program
        |> translateProgramText 
        |> emitOutcome
        |> (_ match {
          case Left(errorMessage) =>
            println(errorMessage)

          case Right(program) =>
            program foreach reportLine
        })
      )
    case failure =>
      println(failure)
  }
}