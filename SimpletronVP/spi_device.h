#pragma once
#include <systemc.h>
#include <array>

SC_MODULE(SpiDevice)
{
    sc_in<bool> sclk;
    sc_in<bool> ce;
    sc_out<bool> miso;
    sc_in<bool> mosi;

    unsigned short shift_counter = 0;
    unsigned short shift_reg = 0x8002; // b 1111 1011

    bool _mosi;

    void update()
    {
        if (ce)
        {
            if (sclk)
            {

                if (shift_counter >= 16) // ce ^ !sclk ^ sc >= 16
                {
                    miso = static_cast<bool>(shift_reg & 0x01);
                    shift_reg >>= 1;
                    shift_reg |= static_cast<unsigned short>(_mosi) << 15;

                    shift_counter = 0;
                }
                else // ce ^ !sclk ^ sc < 16 
                {
                    miso = static_cast<bool>(shift_reg & 0x01);
                    shift_reg >>= 1;
                    shift_reg |= static_cast<unsigned short>(_mosi) << 15;
                    shift_counter++;
                }
            }
            else if (shift_counter <= 16)
                // ce ^ sclk ^ sc < 16
            {
                _mosi = mosi; 
            }
        } 
    }

    SC_CTOR(SpiDevice)
    {
        SC_METHOD(update);
        sensitive << sclk;
    }
};

