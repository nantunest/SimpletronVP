#pragma once
#include <systemc.h>
#include "mux.h"
#include "spi_device.h"

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
    
    unsigned short AddrOffset = 0x3B;

    enum RegisterAddr {
        ACCEL_XOUT_H = 0x00, 
        ACCEL_XOUT_L, 
        ACCEL_YOUT_H, 
        ACCEL_YOUT_L, 
        ACCEL_ZOUT_H, 
        ACCEL_ZOUT_L, 
        TEMP_OUT_H, 
        TEMP_OUT_L, 
        GYRO_XOUT_H, 
        GYRO_XOUT_L, 
        GYRO_YOUT_H, 
        GYRO_YOUT_L, 
        GYRO_ZOUT_H, 
        GYRO_ZOUT_L, 
        SIZE
    };

    std::array<unsigned short, RegisterAddr::SIZE> register_bank = {0, 1, 0, 0}; // Prescalar default is 1

    unsigned short shift_counter = 0;
    unsigned short shift_reg;
    bool _miso;

    void update()
    {
        if (ce)
        {
            // Read 8 bits as reg address and send regaddress content
            // Change reg address content to new reg address

            if (sclk) {
                if (shift_counter >= 8)
                {
                    mosi = static_cast<bool>(shift_reg & 0x01);
                    shift_reg >>= 1;
                    shift_reg |= static_cast<unsigned short>(_miso) << 15;

                    shift_counter = 0;
                }
                else
                {

                }
            }
        }
    }

    SC_CTOR(Mpu6000)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};
