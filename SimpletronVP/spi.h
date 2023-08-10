#pragma once
#include <systemc.h>
#include <array>

SC_MODULE(Spi)
{
    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<bool> miso;
    sc_out<bool> mosi;
    sc_out<bool> sclk;
    sc_out<bool> ss;

    enum Register {
        STATE = 0x00,       // R    {WAIGING = 0x00, SHIFTING = 0x01}
        PRESCALAR = 0x01,   // RW   {1,2,4,8,16,32}
        COMMAND = 0x02,     // RW   {TRANSFER = 0x01}
        SHIFT = 0x03,       // RW   data [b1..b16] 
        SIZE = 0x04
    };

    enum State {
        WAITING = 0x00,
        SHIFTING = 0x01,
        DONE = 0x02
    };

    enum Command {
        ANY = 0x00,
        TRANSFER = 0x01
    };
    
    static constexpr short base_address = MemoryMux::spi_addr;
    std::array<short, Register::SIZE> register_bank = {0};

    short shift_counter = 0;

    void update()
    {
        const short reg_addr = base_address - address; 

        switch (register_bank[Register::STATE])
        {
            case State::WAITING:
                if (ce) // Configuration
                {
                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (register_bank[Register::COMMAND] == Command::TRANSFER)
                {
                    register_bank[Register::STATE] = State::SHIFTING;
                } 
            break;
            
            case State::SHIFTING:
                if (shift_counter < 16)
                {
                    mosi = static_cast<bool>(register_bank[Register::SHIFT] & 0x01);
                    register_bank[Register::SHIFT] >>= 1;
                    register_bank[Register::SHIFT] |= static_cast<short>(miso) << 15;
                }
                else
                {
                    shift_counter = 0;
                    register_bank[Register::STATE] = Register::DONE;
                }
            break;
        }
    }

    SC_CTOR(Spi)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};