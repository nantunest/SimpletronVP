#pragma once
#include <systemc.h>
#include <array>

SC_MODULE(PrescalarSpi)
{
    sc_in<bool> clk;
    sc_in<bool> ce;
    sc_in<short> prescalar;
    sc_out<bool> sclk;

    short clk_cnt = 0;

    void update()
    {
        if (ce)
        {
            clk_cnt++;
            if (clk_cnt == prescalar)
            {
                clk_cnt = 0;
                sclk = !sclk;
            }
        }
    }
    
    SC_CTOR(PrescalarSpi)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }
};


SC_MODULE(Spi)
{
    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<bool> miso;
    sc_out<bool> mosi;
    sc_out<bool> ss;

    sc_in<bool> sclk;

    sc_out<short> prescalar_reg;
    sc_out<bool> prescalar_ce;

    enum RegisterAddr {
        STATE = 0x00,       // R    {OFF = 0x00, READY = 0x01, SHIFTING = 0x02, DONE = 0x03}
        PRESCALAR = 0x01,   // RW   {1,2,4,8,16,32}
        COMMAND = 0x02,     // RW   {NOP = 0x00, TRANSFER = 0x01}
        SHIFT = 0x03,       // RW   data [b1..b16] 
        SIZE = 0x04
    };

    enum State {
        CONF = 0x00,
        READY = 0x01,
        SHIFTING = 0x02,
        DONE = 0x03
    };

    enum Command {
        NOP = 0x00,
        TRANSFER = 0x01
    };
    
    static constexpr short base_address = MemoryMux::spi_addr;
    std::array<short, RegisterAddr::SIZE> register_bank = {0, 1, 0, 0}; // Prescalar default is 1

    short shift_counter = 0;
    bool _miso = false;

    void update()
    {
        const short reg_addr = address - base_address; 
        std::cout << "[SPI]: address is " << address << std::endl;
        std::cout << "[SPI]: reg_addr is " << reg_addr << std::endl;
        std::cout << "[SPI]: Register sh is " << register_bank[RegisterAddr::SHIFT] << std::endl;
        std::cout << "[SPI]: Register ps is " << register_bank[RegisterAddr::PRESCALAR] << std::endl;
        std::cout << "[SPI]: Register cmd is " << register_bank[RegisterAddr::COMMAND] << std::endl;


        if (clk.posedge()){
            std::cout << "$#$#$#$#$#$#$# CLK POS EDGE" << std::endl;
        }
        else if (clk.negedge())
        {
            std::cout << "%$%$%$%$%$%$%$%$ CLK NEG EDGE" << std::endl;
        }

        if (sclk.posedge()){
            std::cout << "$#$#$#$#$#$#$# SSSSSSCLK POS EDGE" << std::endl;
        }
        else if (sclk.negedge())
        {
            std::cout << "%$%$%$%$%$%$%$%$ SSSSSCLK NEG EDGE" << std::endl;
        }

        ss = false;

        switch (register_bank[RegisterAddr::STATE])
        {
            case State::CONF:
                std::cout << "Conf" << std::endl;
                if (ce) // Configuration
                {
                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (register_bank[RegisterAddr::COMMAND] == Command::TRANSFER)
                {
                    register_bank[RegisterAddr::STATE] = State::READY;
                } 
            break;

            case State::READY:
                std::cout << "Ready" << std::endl;
                prescalar_reg = register_bank[RegisterAddr::PRESCALAR]; 
                prescalar_ce = true;
                register_bank[RegisterAddr::STATE] = State::SHIFTING;

            break;
            
            case State::SHIFTING:
                std::cout << "Shifting" << std::endl;

                if (sclk.posedge())
                {
                    // shift
                    std::cout << "####################$$#$# pos" << std::endl;
                    if (shift_counter < 16)
                    {
                        mosi = static_cast<bool>(register_bank[RegisterAddr::SHIFT] & 0x01);
                        register_bank[RegisterAddr::SHIFT] >>= 1;
                        register_bank[RegisterAddr::SHIFT] |= static_cast<short>(_miso) << 15;
                        shift_counter++;
                    }
                    else
                    {
                        shift_counter = 0;
                        register_bank[RegisterAddr::STATE] = State::DONE;
                    }
                }
                else if (sclk.negedge())
                {
                    // sample
                    std::cout << "#############$$#$# neg" << std::endl;
                    _miso = miso;
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