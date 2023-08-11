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

    enum RegisterAddr {
        STATE = 0x00,       // R    {READY = 0x00, SHIFTING = 0x01, DONE = 0x02}
        PRESCALAR = 0x01,   // RW   {1,2,4,8,16,32}
        COMMAND = 0x02,     // RW   {NOP = 0x00, TRANSFER = 0x01}
        SHIFT = 0x03,       // RW   data [b1..b16] 
        SIZE = 0x04
    };

    enum State {
        READY = 0x00,
        SHIFTING = 0x01,
        DONE = 0x02
    };

    enum Command {
        NOP = 0x00,
        TRANSFER = 0x01
    };
    
    static constexpr short base_address = MemoryMux::spi_addr;
    std::array<short, RegisterAddr::SIZE> register_bank = {0, 1, 0, 0}; // Prescalar default is 1

    short shift_counter = 0;
    short clk_cnt = 0;

    void update()
    {
        const short reg_addr = address - base_address; 
        std::cout << "[SPI]: address is " << address << std::endl;
        std::cout << "[SPI]: reg_addr is " << reg_addr << std::endl;
        std::cout << "[SPI]: Register sh is " << register_bank[RegisterAddr::SHIFT] << std::endl;
        std::cout << "[SPI]: Register ps is " << register_bank[RegisterAddr::PRESCALAR] << std::endl;
        std::cout << "[SPI]: Register cmd is " << register_bank[RegisterAddr::COMMAND] << std::endl;

        sclk = false;
        ss = false;

        switch (register_bank[RegisterAddr::STATE])
        {
            case State::READY:
                std::cout << "Ready" << std::endl;
                if (ce) // Configuration
                {
                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (register_bank[RegisterAddr::COMMAND] == Command::TRANSFER)
                {
                    register_bank[RegisterAddr::STATE] = State::SHIFTING;
                } 
            break;
            
            case State::SHIFTING:
                std::cout << "Shifting" << std::endl;
                if (register_bank[RegisterAddr::PRESCALAR] == clk_cnt)
                {
                    clk_cnt = 0; // Reset prescalar clock counter

                    if (shift_counter < 16)
                    {
                        mosi = static_cast<bool>(register_bank[RegisterAddr::SHIFT] & 0x01);
                        register_bank[RegisterAddr::SHIFT] >>= 1;
                        register_bank[RegisterAddr::SHIFT] |= static_cast<short>(miso) << 15;
                    }
                    else
                    {
                        shift_counter = 0;
                        register_bank[RegisterAddr::STATE] = State::DONE;
                    }
                }
                else{
                    clk_cnt++;
                }
            break;

            case State::DONE:
                std::cout << "Done" << std::endl;
                if (ce && reg_addr == RegisterAddr::STATE && data == State::READY)
                {
                    register_bank[RegisterAddr::STATE] = State::READY;
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