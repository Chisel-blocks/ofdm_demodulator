
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
        users : Int
    ) extends Bundle {
        val A = Input(DspComplex(
                SInt((n).W),
                SInt((n).W)
            ) 
        )
        val Z = Output(DspComplex(
                SInt((n).W),
                SInt((n).W)
            )
        )

        val equalize_sync=Input(Bool()) //Rising edge resets equalization counters
        override def cloneType = (new channel_equalizer_io(
                n=n,
                symbol_length=symbol_length,
                users=users
            )
        ).asInstanceOf[this.type]

   }

class ofdm_demodulator[T <:Data] (
        n: Int, 
        symbol_length: Int, 
        cyclic_prefix_length: Int) 
    extends Module {
    val io = IO(new ofdm_demodulator_io( proto=proto, n=n))
    val register=RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(proto.cloneType))))
    register:=io.A
    io.B:=register

    fftConfig = FixedFFTConfig(
    IOWidth       = n, 
    binaryPoint   = options("binaryPoint").toInt,
    n             = symbol_lenght 
    pipelineDepth = options("pipelineDepth").toInt,
    lanes         = options("lanes").toInt, // number of input
    quadrature    = options("quadrature").toBoolean,
    inverse       = options("inverse").toBoolean, // do inverse fft when true
    unscrambleOut = options("unscrambleOut").toBoolean, //  correct output bit-order, only functional when (n=lanes)
    unscrambleIn  = options("unscrambleIn").toBoolean // accept srambled input
  )
 
    fft=Module( new 


}

//This gives you verilog
object ofdm_demodulator extends App {
    chisel3.Driver.execute(args, () => new ofdm_demodulator(
        proto=DspComplex(UInt(16.W),UInt(16.W)), n=8)
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: ofdm_demodulator[DspComplex[UInt]] ) extends DspTester(c) {
//Tests are here 
    poke(c.io.A(0).real, 5)
    poke(c.io.A(0).imag, 102)
    step(5)
    fixTolLSBs.withValue(1) {
        expect(c.io.B(0).real, 5)
        expect(c.io.B(0).imag, 102)
    }
}

//This is the test driver 
object unit_test extends App {
    iotesters.Driver.execute(args, () => new ofdm_demodulator(
            proto=DspComplex(UInt(16.W),UInt(16.W)), n=8
        )
    ){
            c=>new unit_tester(c)
    }
}
