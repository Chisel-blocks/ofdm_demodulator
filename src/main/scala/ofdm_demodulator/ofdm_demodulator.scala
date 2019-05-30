
// Dsp-block ofdm_demodulator
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20190522
package ofdm_demodulator

import chisel3.experimental._
import chisel3._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex
import FFT._

class ofdm_demodulator_io( 
        n : Int, 
        symbol_length : Int, 
    ) extends Bundle {
        val A = Input(DspComplex(
                SInt((n).W),
                SInt((n).W)
            ) 
        )
        val symbol_sync = Input(Bool()) 

        val Z = Output(DspComplex(
                SInt((n).W),
                SInt((n).W)
            )
        )

        val equalize_sync=Input(Bool()) //Rising edge resets equalization counters
        override def cloneType = (new ofdm_demodulator_io(
                n=n,
                symbol_length=symbol_length,
            )
        ).asInstanceOf[this.type]

   }

class ofdm_demodulator[T <:Data] (
        n: Int, 
        symbol_length: Int, 
        cyclic_prefix_length: Int) 
    extends Module {
    val io = IO(new ofdm_demodulator_io( n=n, symbol_length=symbol_length ))
    val register=RegInit(0.U.asTypeOf(io.A))
    register:=io.A
    io.Z:=register

    val fftConfig = FixedFFTConfig(
    IOWidth       = n, 
    binaryPoint   = 1,
    n             = symbol_length,
    pipelineDepth = 0,
    lanes         = symbol_length,
    quadrature    = false,
    inverse       = false, // do inverse fft when true
    unscrambleOut = true, //  correct output bit-order, only functional when (n=lanes)
    unscrambleIn  = false   // accept srambled input
  )
 
    val fft=Module( new FFT(fftConfig)) 
    fft.io.in.bits.map(_.real:=io.A.real.asFixedPoint(1.BP))
    fft.io.in.bits.map(_.imag:=io.A.imag.asFixedPoint(1.BP))
    fft.io.in.sync:=io.symbol_sync
    fft.io.in.valid:=true.B
    fft.io.data_set_end_clear:=false.B


}

//This gives you verilog
object ofdm_demodulator extends App {
    chisel3.Driver.execute(args, () => new ofdm_demodulator(
        n=8, symbol_length=64, cyclic_prefix_length=16)
    )
}

////This is a simple unit tester for demonstration purposes
//class unit_tester(c: ofdm_demodulator[DspComplex[UInt]] ) extends DspTester(c) {
////Tests are here 
//    poke(c.io.A(0).real, 5)
//    poke(c.io.A(0).imag, 102)
//    step(5)
//    fixTolLSBs.withValue(1) {
//        expect(c.io.B(0).real, 5)
//        expect(c.io.B(0).imag, 102)
//    }
//}
//
////This is the test driver 
//object unit_test extends App {
//    iotesters.Driver.execute(args, () => new ofdm_demodulator(
//            proto=DspComplex(UInt(16.W),UInt(16.W)), n=8
//        )
//    ){
//            c=>new unit_tester(c)
//    }
//}
