package chip8
package model

object InstructionSet {
  sealed trait Instruction
  final case object Cls 
    extends Instruction
  final case object Ret
    extends Instruction
  final case class Sys(address: Short) 
    extends Instruction
  final case class Jp(address: Short) 
    extends Instruction
  final case class JpV0(address: Short) 
    extends Instruction
  final case class Call(address: Short) 
    extends Instruction
  sealed trait Operand
  final case class Immediate(value: Short) 
    extends Operand
  final case class Register(index: Short)
    extends Operand
  final case object Index
    extends Operand
  final case object DelayTimer
    extends Operand
  final case object SoundTimer
    extends Operand
  final case object WaitForKeyboard
    extends Operand
  final case class DigitSprite(source: Register)
    extends Operand
  final case class Se(register: Short, source: Operand) 
    extends Instruction
  final case class Sne(register: Short, value: Operand) 
    extends Instruction
  final case class Ld(target: Operand, source: Operand)
    extends Instruction
  final case class LdBcd(source: Register)
    extends Instruction
  final case class Add(target: Operand, source: Operand)
    extends Instruction
  final case class Or(register0: Short, register1: Short)
    extends Instruction
  final case class And(register0: Short, register1: Short)
    extends Instruction
  final case class Xor(register0: Short, register1: Short)
    extends Instruction
  final case class Sub(register0: Short, register1: Short)
    extends Instruction
  final case class Subn(x: Short, y: Short)
    extends Instruction
  final case class Shr(x: Short, y: Short)
    extends Instruction
  final case class Shl(x: Short, y: Short)
    extends Instruction
  final case class SaveRegs(untilReg: Short)
    extends Instruction
  final case class RestoreRegs(untilReg: Short)
    extends Instruction
  final case class Rnd(x: Short, immediate: Short)
    extends Instruction
  final case class Drw(x: Short, y: Short, n: Short)
    extends Instruction
  final case class Skp(x: Short)
    extends Instruction
  final case class Sknp(x: Short)
    extends Instruction
  final case object Halt
    extends Instruction
}