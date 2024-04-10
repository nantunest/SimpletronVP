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

    static constexpr int B_n = 16;
    static constexpr int B_1 = 1;
    
    void sample()
    {
        _mosi = mosi;
    }

    void shift()
    {
        miso = static_cast<bool>(shift_reg & B_1);
        shift_reg >>= 1;
        shift_reg |= static_cast<unsigned short>(_mosi) << (B_n - 1);
        shift_counter >= B_n ? shift_counter = 0 : shift_counter++;
    }

    void update()
    {
        if (ce)
        {
            if (sclk)
            {
                shift();
            }
            else if (shift_counter <= B_n)
            {
                sample();
            }
        } 
    }

    SC_CTOR(SpiDevice)
    {
        SC_METHOD(update);
        sensitive << sclk;
    }
};

