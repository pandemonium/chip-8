package chip8
package emulator

import Function.const

case class Memory(data: Vector[Short]) {
  def read(offset: Short): Short = 
    data(offset).asInstanceOf[Short]
  
  def readMultiple(offset: Short, length: Int) =
    data.slice(offset, offset + length)
  
  def withWrite(offset: Short)(value: Short) =
    copy(data.updated(offset, value))

  def withWrites(offset: Short)(values: Vector[Short]) = 
    copy(data.patch(offset, values, values.length))
}

case class CpuState(v: Vector[Short], 
                    i: Short,
                   sp: Short,
                   pc: Short) {
  def mapPc(f: Short => Short): CpuState =
    copy(pc = f(pc))
  
  def save(target: Memory): Memory =
    target.withWrites(i)(v)

  def restore(source: Memory): CpuState = 
    copy(v = source.readMultiple(i, v.length))
}

case class Keyboard() {
  def keyDown: Option[Short] = Option(3)
}

case class Machine(memory: Memory, 
                 cpuState: CpuState,
                 keyboard: Keyboard,
                   halted: Boolean) {
  def mapPc(f: Short => Short): Machine =
    copy(cpuState = cpuState mapPc f)

  def next = mapPc(Chip8.Next)
  def at   = memory read cpuState.pc

  def push(w: Short): Machine = {
    val sp1       = (cpuState.sp + 1).asInstanceOf[Short]
    val cpuState1 = cpuState.copy(sp = sp1)
    val memory1   = memory.withWrite(sp1)(w)

    copy(cpuState = cpuState1,
         memory   = memory1)
  }

  def pop: (Short, Machine) = {
    val sp1       = (cpuState.sp + 1).asInstanceOf[Short]
    val cpuState1 = cpuState.copy(sp = sp1)

    (memory read cpuState.sp, 
     copy(cpuState = cpuState1))
  }

  def apply(in: Chip8.InstructionT): Machine =
    in apply this

  def saveRegistersToMemory: Machine =
    copy(memory = cpuState save memory)

  def restoreRegistersFromMemory: Machine =
    copy(cpuState = cpuState restore memory)

  def loadCode(toOffset: Short)(code: Vector[Short]): Machine =
    copy(memory = memory.withWrites(toOffset)(code))
}

object Chip8 {
  type InstructionT = Machine => Machine

  object InstructionParser extends `binary-parser`.Parsers {
    import model.InstructionSet._

    def halt   = word(0x0000, 4)       ^^ { _                  => Halt }
    def cls    = word(0x00E0, 4)       ^^ { _                  => Cls }
    def ret    = word(0x00EE, 4)       ^^ { _                  => Ret }
    def sys    = 0 ~ !3                ^^ { case _ ~ address   => Sys(address) }
    def jp     = 1 ~ !3                ^^ { case _ ~ address   => Jp(address) }
    def jpRel  = 0xB ~ !3              ^^ { case _ ~ address   => JpV0(address) }
    def call   = 2 ~ !3                ^^ { case _ ~ address   => Call(address) }
    def se     = 3 ~ !1 ~ !2           ^^ { case _ ~ x ~ byte  => Se(x, Immediate(byte) ) }
    def sneImm = 4 ~ !1 ~ !2           ^^ { case _ ~ x ~ byte  => Sne(x,Immediate(byte)) }
    def sneReg = 9 ~ !1 ~ !1 ~ 0       ^^ { case _ ~ x ~ y     => Sne(x, Register(y)) }
    def ldImm  = 6 ~ !1 ~ !2           ^^ { case _ ~ x ~ byte  => Ld(Register(x), Immediate(byte)) }
    def ldReg  = 8 ~ !1 ~ !1 ~ 0       ^^ { case _ ~ x ~ y ~ _ => Ld(Register(x), Register(y)) }
    def ldI    = 0xA ~ !3              ^^ { case _ ~ address   => Ld(Index, Immediate(address)) }
    def lddt   = 0xF ~ !1 ~ byte(0x07) ^^ { case _ ~ x ~ _     => Ld(Register(x), DelayTimer) }
    def ldk    = 0xF ~ !1 ~ byte(0x0A) ^^ { case _ ~ x ~ _     => Ld(Register(x), WaitForKeyboard) }
    def lddt2  = 0xF ~ !1 ~ byte(0x15) ^^ { case _ ~ x ~ _     => Ld(DelayTimer, Register(x)) }
    def ldst   = 0xF ~ !1 ~ byte(0x18) ^^ { case _ ~ x ~ _     => Ld(SoundTimer, Register(x)) }
    def ldF    = 0xF ~ !1 ~ byte(0x29) ^^ { case _ ~ x ~ _     => Ld(Index, DigitSprite(Register(x))) }
    def ldBcd  = 0xF ~ !1 ~ byte(0x33) ^^ { case _ ~ x ~ _     => LdBcd(Register(x)) }
    def saveR  = 0xF ~ !1 ~ byte(0x55) ^^ { case _ ~ x ~ _     => SaveRegs(x) }
    def restR  = 0xF ~ !1 ~ byte(0x65) ^^ { case _ ~ x ~ _     => RestoreRegs(x) }
    def addImm = 7 ~ !1 ~ !2           ^^ { case _ ~ x ~ byte  => Add(Register(x), Immediate(byte)) }
    def addReg = 8 ~ !1 ~ !1 ~ 4       ^^ { case _ ~ x ~ y ~ _ => Add(Register(x), Register(y)) }
    def addI   = 0xF ~ !1 ~ byte(0x1E) ^^ { case _ ~ x ~ _     => Add(Index, Register(x)) }
    def or     = 8 ~ !1 ~ !1 ~ 1       ^^ { case _ ~ x ~ y ~ _ => Or(x, y) }
    def and    = 8 ~ !1 ~ !1 ~ 2       ^^ { case _ ~ x ~ y ~ _ => And(x, y) }
    def xor    = 8 ~ !1 ~ !1 ~ 3       ^^ { case _ ~ x ~ y ~ _ => Xor(x, y) }
    def sub    = 8 ~ !1 ~ !1 ~ 5       ^^ { case _ ~ x ~ y ~ _ => Sub(x, y) }
    def shr    = 8 ~ !1 ~ !1 ~ 6       ^^ { case _ ~ x ~ y ~ _ => Shr(x, y) }
    def subn   = 8 ~ !1 ~ !1 ~ 7       ^^ { case _ ~ x ~ y ~ _ => Subn(x, y) }
    def shl    = 8 ~ !1 ~ !1 ~ 0xE     ^^ { case _ ~ x ~ y ~ _ => Shl(x, y) }
    def rnd    = 0xC ~ !1 ~ !2         ^^ { case _ ~ x ~ byte  => Rnd(x, byte) }
    def drw    = 0xD ~ !1 ~ !1 ~ !1    ^^ { case _ ~ x ~ y ~ n => Drw(x, y, n) }
    def skp    = 0xE ~ !1 ~ byte(0x9E) ^^ { case _ ~ x ~ _     => Skp(x) }
    def sknp   = 0xE ~ !1 ~ byte(0xA1) ^^ { case _ ~ x ~ _     => Sknp(x) }

    def instructionP = halt | cls | sys | jp | call | se | sneImm | ldImm | addImm | ldReg | or | and | xor | addReg | sub | shr
    def code         = many(instructionP)
  }

  final val Next: Short => Short = (pc: Short) => (pc + 1).asInstanceOf[Short]
  final val Goto: Short => Short => Short = (const: Short) => (pc: Short) => const

  def emptyMemory =
    Memory(Vector.fill[Short](4096)(0))

  def emptyRegisters =
    CpuState(Vector.fill[Short](16)(0), 0, 0, 0)

  def emptyMachine = 
    Machine(emptyMemory, 
            emptyRegisters,
            Keyboard(),
            halted = false)

  import InstructionParser._
  def decodeNext: InstructionT = state => {
    InstructionParser.run(instructionP)(Vector(state.at)) match {
      case Success(ins, _) =>
        interpret(ins)(state)
      case Failure(_) => 
        println("Failed and now halted.")
        state.copy(halted = true)
    }
  }

  import model.InstructionSet._
  def resolveTarget(op: Operand)(machine: Machine) = op match {
    case Immediate(address) =>
      (value: Word) => 
        machine.copy(memory = machine.memory.withWrite(address)(value))
    case Register(x) =>
      (value: Word) =>
        val v1 = machine.cpuState.v.updated(x, (value & 0xFF).asInstanceOf[Word])

        machine.copy(cpuState = machine.cpuState.copy(v = v1))
    case SoundTimer =>
      ???
    case DelayTimer =>
      ???
    case Index =>
      (value: Word) =>
        machine.cpuState.copy(i = value)
      ???
    case WaitForKeyboard =>
      ???
    case DigitSprite(register) =>
      ???
  }

  def resolveSource(op: Operand)(machine: Machine) = op match {
    case Immediate(const) => const
    case Register(x)      => machine.cpuState.v(x)
    case SoundTimer =>
      ???
    case DelayTimer =>
      ???
    case Index =>
      machine.cpuState.i
    case WaitForKeyboard =>
      ???
    case DigitSprite(register) =>
      ???    
  }

  def binop(target: Operand, source: Operand)(op: (Word, Word) => Word): InstructionT = state => {
    val operand0 = resolveSource(target) _
    val operand1 = resolveSource(source) _
    val target1  = resolveTarget(target) _

    target1(state)(op(operand0(state), operand1(state)))
  }

  def word(x: Int): Word = x.asInstanceOf[Word]

  def interpret(instruction: Instruction)(state: Machine): Machine = instruction match {
    case Halt =>
      state.copy(halted = true)

    case Cls =>
      println("Clear screen!")
      state.next

    case Sys(address) =>
      println(s"Call system @ ${Integer.toString(address, 16)}")
      state.next

    case Ld(target, source) =>
      resolveTarget(target)(state)(resolveSource(source)(state)).next

    case And(target, source) =>
      binop(Register(target), Register(source))((a, b) => word(a & b))(state).next
    case Or(target, source) =>
      binop(Register(target), Register(source))((a, b) => word(a | b))(state).next
    case Xor(target, source) =>
      binop(Register(target), Register(source))((a, b) => word(a ^ b))(state).next

    case Add(target, source) =>
      binop(target, source)((a, b) => word(a + b))(state).next
    case Sub(target, source) =>
      // Also set VF to 1 if Vx > Vy, 0 otherwise
      binop(Register(target), Register(source))((a, b) => word(a - b))(state).next
    case Subn(target, source) =>
      // Also set VF to 1 if Vx > Vy, 0 otherwise
      binop(Register(target), Register(source))((a, b) => word(b - a))(state).next

    case Shl(target, _) =>
      binop(Register(target), Immediate(0))((a, _) => word(a << 1))(state).next
    case Shr(target, _) =>
      binop(Register(target), Immediate(0))((a, _) => word(a >> 1))(state).next

    case Call(address) =>
      state push state.cpuState.pc mapPc Goto(address)

    case Ret =>
      state.pop match {
        case (address, state1) =>
          state1 mapPc Goto(address)
      }

    case Jp(address) =>
      state mapPc Goto(address)

    case JpV0(address) =>
      val v0 = resolveSource(Register(0))(state)

      state mapPc Goto(word(v0 + address))

    case Se(x, y) =>
      val op0 = resolveSource(Register(x))(state)
      val op1 = resolveSource(y)(state)

      if (op0 == op1) state.next.next
      else state.next

    case Sne(x, y) =>
      val op0 = resolveSource(Register(x))(state)
      val op1 = resolveSource(y)(state)

      if (op0 != op1) state.next.next
      else state.next

    case Skp(x) =>
      val key = resolveSource(Register(x))(state)

      if (state.keyboard.keyDown exists key.==)
        state.next.next
      else 
      state.next
    
    case Sknp(x) =>
      val key = resolveSource(Register(x))(state)

      if (state.keyboard.keyDown exists key.==)
        state.next
      else
        state.next.next

    case Rnd(x, y) =>
      def rnd = scala.util.Random nextInt 255

      binop(Register(x), Immediate(y))((a, b) => word(rnd & b))(state).next

    case SaveRegs(x) =>
      state.saveRegistersToMemory.next
    
    case RestoreRegs(x) =>
      state.restoreRegistersFromMemory.next

    case Drw(x, y, n) =>
      ???

    case LdBcd(x) =>
      ???
  }

  def execute(machine: Machine): Machine =
    if (!machine.halted)
      execute(machine(decodeNext))
    else 
      machine
}

trait Show[A] {
  def show(a: A): String
}

object Implicits {
  import model.InstructionSet._
  implicit object InstructionShow extends Show[Instruction] {
    def show(i: Instruction): String = i match {
      case Ld(Register(x), Register(y)) =>
        s"LD V$x, V$y"
      case _ =>
        s"Unknown instruction: $i"
    }
  }
}

object Main extends App {
  println("Trying the parsor")

/*
    LD	V0, #10
    LD	V1, #2
    LD	V2, #1
Loop:
    SUB	V0, V2
    ADD V1, #2
    SE	V0, #0
    JP	Loop

    HLT
*/

  def emit(opCode: Int): Short = opCode.asInstanceOf[Short]

  val opCodes = Vector(emit(0x600A), 
                       emit(0x6102),
                       emit(0x6201),
                       emit(0x8025),
                       emit(0x7102),
                       emit(0x3000),
                       emit(0x1103),
                       emit(0x0000))

  import Chip8.InstructionParser._
  val result = run(code)(opCodes) 
  println(result)

  val machine  = Chip8.emptyMachine.loadCode(0x100)(opCodes)
  val machine1 = Chip8 execute machine.mapPc(Chip8 Goto 0x100)

  println(machine1.cpuState)
}