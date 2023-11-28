#pragma once
#include <systemc.h>

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

    std::array<unsigned short, RegisterAddr::SIZE> register_bank = {0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE};

    unsigned char shift_counter = 0;
    unsigned char shift_reg = 0x00;

    static constexpr int B_n = 8;
    static constexpr int B_1 = 1;

    static constexpr int base_addr = 0x3B;

    void write_bit()
    {
        miso = static_cast<bool>(shift_reg & B_1);
    }

    void shift()
    {
        shift_reg >>= 1;
        shift_reg |= static_cast<unsigned char>(mosi) << (B_n - 1);
        shift_counter++;

   }

    void update()
    {
        if (ce)
        {
            if (sclk)
            {
                shift();
                if (shift_counter >= B_n)
                { 
                    shift_counter = 0;
                    std::cout << "[MPU6000]: shift_reg = " << shift_reg <<" " << "content = " << register_bank[shift_reg - base_addr] << std::endl; 
                    shift_reg = register_bank[shift_reg - base_addr];
                }
                // else
                // {
                //     shift();
                // }
 
            }
            else
            {
                write_bit();
            }
        }
    }

    SC_CTOR(Mpu6000)
    {
        SC_METHOD(update);
        sensitive << sclk;
    }

};
