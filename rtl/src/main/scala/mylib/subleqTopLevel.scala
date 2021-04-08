package mylib

import spinal.core._
import spinal.lib.fsm._
import spinal.lib.misc.BinTools
import spinal.lib.com.uart._
import spinal.lib.master
import spinal.lib.Stream

class subleqTopLevel(frequency: FixedFrequency) extends Component {
  val io = new Bundle {
    val clk     = in Bool
    val resetn  = in Bool

    val uart = master(Uart())
  }

  val subleqClockDomain = ClockDomain(
    clock  = io.clk,
    reset  = io.resetn,
    frequency = frequency,
    config = ClockDomainConfig(
      clockEdge        = RISING,
      resetKind        = ASYNC,
      resetActiveLevel = LOW
    )
  )
  val subleqArea = new ClockingArea(subleqClockDomain) {

    val uartCtrl = new UartCtrl()
    uartCtrl.io.config.setClockDivider(HertzNumber(115200))
    uartCtrl.io.config.frame.dataLength := 7  //8 bits
    uartCtrl.io.config.frame.parity := UartParityType.NONE
    uartCtrl.io.config.frame.stop := UartStopType.ONE
    uartCtrl.io.writeBreak := False
    io.uart <> uartCtrl.io.uart
    val uart_write_valid  = Reg(Bool) init(False)
    val uart_write_payload  = Reg(Bits(8 bits))
    val write = Stream(Bits(8 bits))
    write.valid := uart_write_valid
    write.payload := uart_write_payload
    write >-> uartCtrl.io.write
    val uart_read_ready = Reg(Bool) init(False)
    uartCtrl.io.read.ready := uart_read_ready

    val ram     = Mem(Bits(16 bits), 32768)
    val ram_in  = Reg(Bits(16 bits))
    val ram_we  = Reg(Bool) init(False)
    val ram_out = Bits(16 bits)
    val ram_addr = Reg(UInt(15 bits))
    ram_out := ram.readSync(ram_addr)
    ram.write(ram_addr,ram_in,ram_we)
    BinTools.initRam(ram,"../out.bin")

    val program_counter = Reg(UInt(16 bits)) init(0)
    val A_value = Reg(UInt(16 bits))
    val A_addr  = Reg(UInt(16 bits))
    val B_addr  = Reg(UInt(16 bits))

    val main_fsm = new StateMachine {
      ram_we := False
      val fetch_A_addr : State = new State with EntryPoint {
        whenIsActive {
          when(program_counter.asSInt =/= -1){ //Halt if pc = -1
            ram_addr := program_counter.resized
            program_counter := program_counter + 1

            goto(fetch_A_addr_delay)
          }
        }
      }
      val fetch_A_addr_delay : State = new StateDelay(1){whenCompleted(goto(fetch_A_value))}
      val fetch_A_value : State = new State {
        whenIsActive {
          A_addr := ram_out.asUInt
          ram_addr := ram_out.asUInt.resized
          goto(fetch_A_value_delay)
        }
      }
      val fetch_A_value_delay : State = new StateDelay(1){whenCompleted(goto(fetch_B_addr))}
      val fetch_B_addr : State = new State {
        whenIsActive {
          A_value := ram_out.asUInt

          ram_addr := program_counter.resized
          program_counter := program_counter + 1
          goto(fetch_B_addr_delay)
        }
      }
      val fetch_B_addr_delay : State = new StateDelay(1){whenCompleted(goto(fetch_B_value))}
      val fetch_B_value : State = new State {
        whenIsActive {
          B_addr := ram_out.asUInt
          ram_addr := ram_out.asUInt.resized
          goto(fetch_B_value_delay)
        }
      }
      val fetch_B_value_delay : State = new StateDelay(1){whenCompleted(goto(execute))}
      val execute : State = new State {
        whenIsActive {
          program_counter := program_counter + 1
          goto(fetch_A_addr)

          when(A_addr.asSInt === -1){ //UART Input
            uart_read_ready := True
            goto(uart_read_state)
          }.elsewhen(B_addr.asSInt === -1){ //UART Output
            uart_write_valid := True
            uart_write_payload := A_value.asBits.resized
            goto(uart_write_state)
          }.otherwise{
            ram_in := (ram_out.asUInt - A_value).asBits
            ram_addr := B_addr.resized
            ram_we := True

            when(ram_out.asSInt - A_value.asSInt <= 0) {
              program_counter := program_counter //Dont increment pc
              goto(jump_to_C_1)
            }
          }
        }
      }
      val jump_to_C_1 : State = new State {
        whenIsActive {
          ram_addr := program_counter.resized
          goto(jump_to_C_delay)
        }
      }
      val jump_to_C_delay : State = new StateDelay(1){whenCompleted(goto(jump_to_C_2))}
      val jump_to_C_2 : State = new State {
        whenIsActive {
          program_counter := ram_out.asUInt
          goto(fetch_A_addr)
        }
      }
      val uart_write_state : State = new State {
        whenIsActive {
          when(write.ready){
            uart_write_valid := False
            goto(fetch_A_addr)
          }
        }
      }
      val uart_read_state : State = new State {
        whenIsActive {
          when(uartCtrl.io.read.valid){
            uart_read_ready := False
            ram_in := uartCtrl.io.read.payload.resized
            ram_addr := B_addr.resized
            ram_we := True
            goto(fetch_A_addr)
          }
        }
      }
    }
  }
}

object subleqLevelVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new subleqTopLevel(FixedFrequency(24 MHz)))
  }
}