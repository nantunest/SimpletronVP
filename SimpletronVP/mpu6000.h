#pragma once
#include <systemc.h>
#include "mux.h"

/*
//----------------------------------------------------------------//

    Addr (hex)  Addr (dec)      Register Name   RW  Bitfield
    3B          59              ACCEL_XOUT_H    R   ACCEL_XOUT[15:8]
    3C          60              ACCEL_XOUT_L    R   ACCEL_XOUT[7:0]
    3D          61              ACCEL_YOUT_H    R   ACCEL_YOUT[15:8]
    3E          62              ACCEL_YOUT_L    R   ACCEL_YOUT[7:0]
    3F          63              ACCEL_ZOUT_H    R   ACCEL_ZOUT[15:8]
    40          64              ACCEL_ZOUT_L    R   ACCEL_ZOUT[7:0]
    41          65              TEMP_OUT_H      R   TEMP_OUT[15:8]
    42          66              TEMP_OUT_L      R   TEMP_OUT[7:0]
    43          67              GYRO_XOUT_H     R   GYRO_XOUT[15:8]
    44          68              GYRO_XOUT_L     R   GYRO_XOUT[7:0]
    45          69              GYRO_YOUT_H     R   GYRO_YOUT[15:8]
    46          70              GYRO_YOUT_L     R   GYRO_YOUT[7:0]
    47          71              GYRO_ZOUT_H     R   GYRO_ZOUT[15:8]
    48          72              GYRO_ZOUT_L     R   GYRO_ZOUT[7:0]

//-----------------------------------------------------------------//
*/
SC_MODULE(Mpu6000)
{
    sc_in<bool> sclk;
    sc_in<bool> ce;
    sc_out<bool> miso;
    sc_in<bool> mosi;

    void update()
    {
        if (ce)
        {
        }
    }

    SC_CTOR(Mpu6000)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};
