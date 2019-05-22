
// Dsp-block ofdm_demodulator
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20190522
package ofdm_demodulator

import chisel3.experimental._
import chisel3._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex

class ofdm_demodulator_io[T <:Data](proto: T,n: Int)
   extends Bundle {
        val A       = Input(Vec(n,proto))
        val B       = Output(Vec(n,proto))
        override def cloneType = (new ofdm_demodulator_io(proto.cloneType,n)).asInstanceOf[this.type]
   }

class ofdm_demodulator[T <:Data] (proto: T,n: Int) extends Module {
    val io = IO(new ofdm_demodulator_io( proto=proto, n=n))
    val register=RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(proto.cloneType))))
    register:=io.A
    io.B:=register
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
