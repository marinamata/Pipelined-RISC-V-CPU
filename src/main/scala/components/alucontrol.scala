// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  // Your code goes here

  //Rtype code for test 1

  when(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0000000".U) { io.operation := "b00001".U} // 64 ADD 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0000001".U) { io.operation := "b00110".U}// 64 MUL
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0100000".U) { io.operation := "b00100".U}// 64 SUB
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b001".U && io.funct7 === "b0000000".U) {io.operation := "b10010".U}// 64 SLL
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b001".U && io.funct7 === "b0000001".U) {io.operation := "b00111".U}// 64 MULH 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b010".U && io.funct7 === "b0000000".U) {io.operation := "b10110".U}// 64 SLT 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b010".U && io.funct7 === "b0000001".U) {io.operation := "b11000".U}// 64 MULHSU 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b011".U && io.funct7 === "b0000000".U) {io.operation := "b10111".U}// 64 SLTU 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b011".U && io.funct7 === "b0000001".U) { io.operation := "b01000".U}// 64 MULHU 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b100".U && io.funct7 === "b0000000".U) {io.operation := "b01111".U }// 64 XOR 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b100".U && io.funct7 === "b0000001".U) { io.operation := "b01011".U} // 64 DIV 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0000000".U) {io.operation := "b10100".U}// 64 SRL 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0000001".U) {io.operation := "b01010".U}// 64 DIVU 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0100000".U) {io.operation := "b10000".U}// 64 SRA 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b110".U && io.funct7 === "b0000000".U) {io.operation := "b01110".U}// 64 OR 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b110".U && io.funct7 === "b0000001".U) { io.operation := "b11100".U} // 64 REM 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b111".U && io.funct7 === "b0000000".U) {io.operation := "b01101".U}// 64 AND 
  .elsewhen(io.aluop === "b001".U && io.funct3 === "b111".U && io.funct7 === "b0000001".U) {io.operation := "b11011".U}// 64 REMU 
  
  //This is for the 32 bit instructions
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0000000".U) {io.operation := "b00000".U}// 32 ADDW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0000001".U) {io.operation := "b00101".U}// 32 MULW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0100000".U) {io.operation := "b00010".U}// 32 SUBW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b001".U && io.funct7 === "b0000000".U) {io.operation := "b10011".U} // 32 SLLW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b100".U && io.funct7 === "b0000001".U) {io.operation := "b01001".U}// 32 DIVW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0000000".U) {io.operation := "b10101".U}// 32 SRLW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0000001".U) {io.operation := "b01100".U } // 32 DIVUW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0100000".U) { io.operation := "b10001".U}// 32 SRAW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b110".U && io.funct7 === "b0000001".U) {io.operation := "b11010".U}// 32 REMW 
  .elsewhen(io.aluop === "b011".U && io.funct3 === "b111".U && io.funct7 === "b0000001".U) {io.operation := "b11001".U}// 32 REMUW 
  .otherwise{
    io.operation := "b11111".U //invalid
  }

  //I Type instructions for test 2
  when (io.aluop === 2.U){
    when(io.funct3 === "b000".U) {io.operation := "b00001".U} //ADDI
    .elsewhen(io.funct3 === "b111".U) { io.operation := "b01101".U} //ANDI
    .elsewhen(io.funct3 === "b110".U) { io.operation := "b01110".U} //ORI
    .elsewhen(io.funct3 === "b100".U) { io.operation := "b01111".U} //XORI
    .elsewhen(io.funct3 === "b010".U) { io.operation := "b10110".U} //SLTI
    .elsewhen(io.funct3 === "b011".U) { io.operation := "b10111".U} //SLTIU
    .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0100000".U) { io.operation := "b10000".U} //SRAI
    .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0000000".U) { io.operation := "b10100".U} //SRLI
    .elsewhen(io.funct3 === "b001".U) { io.operation := "b10010".U} //SLLI
  }

  //32-bit I-type instructions
  when(io.aluop === 4.U) {
    when(io.funct3 === "b000".U) {io.operation := "b00000".U} //ADDIW
    .elsewhen (io.funct3 === "b101".U) {io.operation := "b10001".U}  //SRAIW
    .elsewhen (io.funct3 === "b101".U) {io.operation := "b10101".U} //SRLIW
    .elsewhen (io.funct3 === "b001".U) {io.operation := "b10011".U} //SLLIW

  }
  //For part 3
  when(io.aluop === 5.U){
  io.operation := "b00001".U

  }
}
