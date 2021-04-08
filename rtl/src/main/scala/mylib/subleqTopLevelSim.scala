package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object subleqTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new subleqTopLevel(FixedFrequency(24 MHz))){dut =>
      dut.subleqClockDomain.forkStimulus(period = 10)
      dut.io.uart.rxd #= true
      sleep(15000)

      val baudPeriod = 2080
      val buffer = 97
      dut.io.uart.rxd #= false
      sleep(baudPeriod)

      for(bitId <- 0 to 7) {
        dut.io.uart.rxd #= ((buffer >> bitId) & 1) != 0
        sleep(baudPeriod)
      }

      dut.io.uart.rxd #= true
      sleep(baudPeriod)

      sleep(100000)
    }
  }
}
