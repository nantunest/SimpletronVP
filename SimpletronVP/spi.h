#pragma once
#include <cstdio>
#include <systemc.h>
#include <array>

SC_MODULE(SpiPrescalar)
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
    sc_in<short> wshift;
    sc_in<bool> miso;
    sc_out<short> rshift;
    sc_out<bool> mosi;

    short shift_counter = 0;
    short shift_reg;

    sc_signal<bool> _miso;

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

                if (shift_counter >= 16) // ce ^ clk ^ !busy ^ sc == 16
                {
                    rshift = shift_reg;
                    shift_counter = 0;
                    busy = false;
                }
                else // ce ^ clk ^ busy ^ sc < 16 
                {
                    mosi = static_cast<bool>(shift_reg & 0x01);
                    shift_reg >>= 1;
                    shift_reg |= static_cast<short>(_miso) << 15;
                    shift_counter++;
                }
            }
            else if (busy && shift_counter < 16)
                // ce ^ !sclk ^ busy ^ sc < 16
            {
                _miso = miso; 
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

    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<bool> miso;
    sc_out<bool> mosi;
    sc_out<bool> ss;
    sc_in<bool> sclk;

    // Internal signals

    sc_signal<bool> _sclk;
    
    SpiPrescalar prescalar;
    sc_signal<short> prescalar_val;
    sc_signal<bool> prescalar_ce;
 
    SpiShifter shifter;
    sc_signal<bool> shifter_ce;
    sc_signal<short> shifter_wshift;
    sc_signal<short> shifter_rshift;
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
        TRANSFER = 0x01
    };
    
    static constexpr short base_address = MemoryMux::spi_addr;
    std::array<short, RegisterAddr::SIZE> register_bank = {0, 1, 0, 0}; // Prescalar default is 1

    short shift_counter = 0;
    bool _miso = false;

    void update()
    {
        const short reg_addr = address - base_address; 
        ss = false;

        switch (register_bank[RegisterAddr::STATE])
        {
            case State::CONF:
                std::cout << "[SPI]: Conf" << std::endl;
                if (ce && reg_addr < RegisterAddr::SIZE) // Configuration
                {
                    std::cout << "[SPI]: address is " << address << std::endl;
                    std::cout << "[SPI]: reg_addr is " << reg_addr << std::endl;
                    std::cout << "[SPI]: Register sh is " << register_bank[RegisterAddr::SHIFT] << std::endl;
                    std::cout << "[SPI]: Register ps is " << register_bank[RegisterAddr::PRESCALAR] << std::endl;


                    // TODO: Validate register address and RW permission
                    register_bank[reg_addr] = data;
                }
                else if (register_bank[RegisterAddr::COMMAND] == Command::TRANSFER)
                {
                    register_bank[RegisterAddr::STATE] = State::READY;
                    std::cout << "[SPI]: Register cmd is " << register_bank[RegisterAddr::COMMAND] << std::endl;
                } 
            break;

            case State::READY:
                std::cout << "[SPI]: Ready" << std::endl;

                prescalar_val = register_bank[RegisterAddr::PRESCALAR]; 
                prescalar_ce = true;
                shifter_ce = true;
                shifter_wshift = register_bank[RegisterAddr::SHIFT];

                register_bank[RegisterAddr::STATE] = State::SHIFTING;

            break;
            
            case State::SHIFTING:
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
                if (ce && reg_addr == RegisterAddr::STATE && data == State::READY)
                {
                    // User-code commands acks spi DONE setting it back to READY.
                    register_bank[RegisterAddr::STATE] = State::READY;
                }
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
        prescalar.sclk(_sclk);

        shifter.sclk(_sclk);
        shifter.ce(shifter_ce);
        shifter.wshift(shifter_wshift);
        shifter.rshift(shifter_rshift);
        shifter.miso(miso);
        shifter.mosi(mosi);
        shifter.busy(shifter_busy);

    }

};
