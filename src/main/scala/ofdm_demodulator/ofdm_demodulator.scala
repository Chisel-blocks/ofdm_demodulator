
// Dsp-block ofdm_demodulator
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20190522
package ofdm_demodulator

import chisel3._
import chisel3.experimental._
import chisel3.util.{ShiftRegister, log2Ceil}
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
        val symbol_sync_in = Input(Bool()) 
        val symbol_sync_out = Output(Bool()) 

        val Z = Output(DspComplex(
                SInt((n).W),
                SInt((n).W)
            )
        )

        override def cloneType = (new ofdm_demodulator_io(
                n=n,
                symbol_length=symbol_length,
            )
        ).asInstanceOf[this.type]

   }

class ofdm_demodulator[T <:Data] (
        n: Int, 
        symbol_length: Int ) 
    extends Module {
        val io = IO(new ofdm_demodulator_io( n=n, symbol_length=symbol_length))
        val register=RegInit(0.U.asTypeOf(io.A))
        register:=io.A
        io.Z:=register
        val pipestages=log2Ceil(symbol_length)
        val fftConfig = FixedFFTConfig(
            IOWidth       = 2*n, 
            binaryPoint   = n,
            n             = symbol_length,
            pipelineDepth = pipestages,
            lanes         = symbol_length,
            quadrature    = false,
            inverse       = false, // do inverse fft when true
            unscrambleOut = true,  //  correct output bit-order, 
                                   // only functional when (n=lanes)
            unscrambleIn  = false  // accept srambled input
        )
     
        val fft=Module( new FFT(fftConfig)).io
        fft.in.bits.map(_:=register.asTypeOf(fft.in.bits(0)))
        val r_fft_sync=ShiftRegister(io.symbol_sync_in,symbol_length)
        fft.in.sync:= r_fft_sync 
        //Does nothing?
        fft.in.valid:= RegNext(r_fft_sync)
        fft.data_set_end_clear:=false.B

        //Serial-to-parllel the input
        val serpa=RegInit(VecInit(Seq.fill(symbol_length){0.U.asTypeOf(io.A)}))
        val serpareg=RegInit(VecInit(Seq.fill(symbol_length){0.U.asTypeOf(fft.in.bits(0))}))

        serpa.last:=io.A
        for ( i <- (1 until serpa.length).reverse) {
            serpa(i-1):=serpa(i)
        }
        when( r_fft_sync ) {
            (serpareg,serpa).zipped.map(_.real:=_.real.asTypeOf(serpareg(0).real))
            (serpareg,serpa).zipped.map(_.imag:=_.imag.asTypeOf(serpareg(0).imag))
        }.otherwise {
        }

        (fft.in.bits,serpareg).zipped.map(_:=_)

        // Parallel-to-serial the FFT output
        val parse=RegInit(VecInit(Seq.fill(symbol_length){0.U.asTypeOf(io.A)}))
        when( fft.out.sync ) {
            (parse,fft.out.bits).zipped.map(_.real:=_.real.asTypeOf(parse(0).real))
            (parse,fft.out.bits).zipped.map(_.imag:=_.imag.asTypeOf(parse(0).imag))
        }.otherwise {
            parse.last:=0.U.asTypeOf(parse(0))
            // Reverse to get bin 0 out first
            for ( i <- (1 until parse.length).reverse) {
                parse(i-1):=parse(i)
            }
        }
        io.symbol_sync_out:=RegNext(fft.out.sync)
        io.Z:=parse(0)
} 


//This gives you verilog
object ofdm_demodulator extends App {
    chisel3.Driver.execute(args, () => new ofdm_demodulator(
        n=16, symbol_length=64)
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
