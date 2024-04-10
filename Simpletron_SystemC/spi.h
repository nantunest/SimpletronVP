#pragma once
#include <systemc.h>
#include <array>
#include "mux.h"

SC_MODULE(SpiPrescalar)
{
    sc_in<bool> clk;
    sc_in<bool> ce;
    sc_in<unsigned short> prescalar;
    sc_out<bool> sclk;

    unsigned short clk_cnt = 0;

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
        else
        {
            sclk = true;
        }
    }
    
    SC_CTOR(SpiPrescalar)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }
};


SC_MODULE(SpiShifter)
{
    sc_out<bool> busy;
    sc_in<bool> sclk;
    sc_in<bool> ce;
    sc_in<unsigned short> wshift;
    sc_in<bool> miso;
    sc_out<unsigned short> rshift;
    sc_out<bool> mosi;

    unsigned short shift_counter = 0;
    unsigned short shift_reg;

    static constexpr int B_n = 16;
    static constexpr int B_1 = 1;

    void write_bit()
    {
        mosi = static_cast<bool>(shift_reg & B_1);
    }

    void shift()
    {
        shift_reg >>= 1;
        shift_reg |= static_cast<unsigned short>(miso) << (B_n - 1);
        shift_counter++;
    }


    void update()
    {
        if (ce)
        {
            // Setting value to shift
            if (busy == false) // ce ^ !busy
            {
                shift_reg = wshift; 
                busy = true;
            }
            
            else if (sclk)
            {
                shift();
                if (shift_counter >= B_n)
                {
                    rshift = shift_reg;
                    shift_counter = 0;
                    busy = false;
                }
            }
            else
            {
                write_bit();
            }
        } 
    }

    SC_CTOR(SpiShifter)
    {
        SC_METHOD(update);
        sensitive << sclk << ce;
    }
};

SC_MODULE(Spi)
{
    // External signals

    sc_in<unsigned short> address;
    sc_inout<unsigned short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;
    sc_in<bool> we;

    sc_in<bool> miso;
    sc_out<bool> mosi;
    sc_out<bool> ss;
    sc_out<bool> sclk;

    // Internal signals

    SpiPrescalar prescalar;
    sc_signal<unsigned short> prescalar_val;
    sc_signal<bool> prescalar_ce;
    
    SpiShifter shifter;
    sc_signal<bool> shifter_ce;
    sc_signal<unsigned short> shifter_wshift;
    sc_signal<unsigned short> shifter_rshift;
    sc_signal<bool> shifter_busy;


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
        TRANSFER = 0x01,
    };
    
    static constexpr unsigned short base_address = MemoryMux::spi_addr;
    std::array<unsigned short, RegisterAddr::SIZE> register_bank = {0, 1, 0, 0}; // Prescalar default is 1

    unsigned short shift_counter = 0;

    void update()
    {
        const unsigned short reg_addr = address - base_address; 

        switch (register_bank[RegisterAddr::STATE])
        {
            case State::CONF:
                if (ce && we && reg_addr < RegisterAddr::SIZE) // Configuration
                {
                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (ce && !we && reg_addr < RegisterAddr::SIZE)
                {
                    data = register_bank[reg_addr];
                }
                else if (register_bank[RegisterAddr::COMMAND] == Command::TRANSFER)
                {
                    register_bank[RegisterAddr::STATE] = State::READY;
                } 
            break;

            case State::READY:
                // In READY state, read from register_bank
                if (ce && !we && reg_addr < RegisterAddr::SIZE){
                    data = register_bank[reg_addr];
                }

                std::cout << "[SPI]: Ready" << std::endl;

                prescalar_val = register_bank[RegisterAddr::PRESCALAR]; 
                prescalar_ce = true;
                shifter_ce = true;
                ss = true;
                shifter_wshift = register_bank[RegisterAddr::SHIFT];

                register_bank[RegisterAddr::STATE] = State::SHIFTING;

            break;
            
            case State::SHIFTING:

                // In SHIFTING state, read from register_bank
                if (ce && !we && reg_addr < RegisterAddr::SIZE){
                    data = register_bank[reg_addr];
                }

                std::cout << "[SPI]: Shifting" << std::endl;
                if (!shifter_busy)
                {
                    register_bank[RegisterAddr::SHIFT] = shifter_rshift;
                    prescalar_ce = false;
                    shifter_ce = false;

                    register_bank[RegisterAddr::STATE] = State::DONE;
                }
                else
                {
                    // Waits for the shifter to finish.
                }

           break;

            case State::DONE:
                std::cout << "[SPI]: Done" << std::endl;
                if (ce && we && reg_addr == RegisterAddr::COMMAND && data == Command::TRANSFER)
                {
                    // User-code commands acks spi DONE setting it back to READY.
                    register_bank[RegisterAddr::STATE] = State::READY;
                }
                else if (ce && we && reg_addr < RegisterAddr::SIZE) // Configuration
                {
                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (ce && !we && reg_addr < RegisterAddr::SIZE)
                {
                    data = register_bank[reg_addr];
                }

                ss = false;
            break;
        }
    }

    SC_CTOR(Spi) : prescalar("prescalar1"), shifter("shifter1")
    {
        SC_METHOD(update);
        sensitive << clk.neg();

        prescalar.clk(clk);
        prescalar.ce(prescalar_ce);
        prescalar.prescalar(prescalar_val);
        prescalar.sclk(sclk);

        shifter.sclk(sclk);
        shifter.ce(shifter_ce);
        shifter.wshift(shifter_wshift);
        shifter.rshift(shifter_rshift);
        shifter.miso(miso);
        shifter.mosi(mosi);
        shifter.busy(shifter_busy);

    }

};
