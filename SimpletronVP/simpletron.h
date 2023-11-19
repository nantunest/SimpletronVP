#pragma once
#include <systemc.h>

SC_MODULE(Simpletron)
{
    sc_in<bool> clk;
    sc_out<unsigned short> address;
    sc_inout<unsigned short> data;
    sc_out<bool> we; // WE = 0 -> Read, WE = 1 -> Write
    
    unsigned short i_address = 0;
    unsigned short i_data = 0;
    bool i_we = false;

    enum State {FETCH, DECODE, EXEC, WRITE_BACK};
    State state = FETCH;
    
    enum OpCodes {SHL       = 0x01,
                  SHR       = 0x02,
                  LOAD      = 0x03,
                  STORE     = 0x04,
                  ADD       = 0x05,
                  SUB       = 0x06,
                  DIV       = 0x07,
                  MUL       = 0x08,
                  JMP       = 0x09,
                  BGZ       = 0x0A,
                  BEZ       = 0x0B,
                  HALT      = 0x0C,
                  SOR       = 0x0D,
                  SAND      = 0x0E,
                 };

    unsigned short accumulator = 0;
    unsigned short fetch_pointer = 0;
    bool initialized = false;
    bool executing = true;
    unsigned short instruction_pointer = 0;

    unsigned short opcode;
    unsigned short operand_addr;
    unsigned short operand_data;

    void shl()
    {
        std::cout << "EXECUTING SHL" << std::endl;
        accumulator <<= operand_data; 
    }

    void shr()
    { 
        std::cout << "EXECUTING SHR" << std::endl;
        accumulator >>= operand_data; 
    }

    void load()
    {
        std::cout << "EXECUTING LOAD" << std::endl;
        accumulator = operand_data;
    }
    
    void store()
    {
        std::cout << "EXECUTING STORE" << std::endl;
        i_data = accumulator;
        i_we = true;
        i_address = operand_addr;
    }
    void halt()
    {
        std::cout << "EXECUTING HALT" << std::endl;
        unsigned short stop;
        executing = false; 
        std::cin >> stop;
    }


    void add()
    {
        std::cout << "EXECUTING ADD" << std::endl;
        accumulator += operand_data; 
    }


    void sub()
    {
        std::cout << "EXECUTING SUB: " << accumulator << " - " << operand_data << std::endl;
        accumulator -= static_cast<unsigned short>(data);
    }

    void div()
    {
        std::cout << "EXECUTING DIV" << std::endl;

        if (data != 0){
            accumulator /= operand_data;
        } else {
            std::cout << "Error: Division by zero"; 
            halt();
        }
    }

    void mul()
    {
        std::cout << "EXECUTING MUL" << std::endl;
        accumulator *= operand_data;
    }

    void jmp()
    {
        std::cout << "EXECUTING BRANCH" << std::endl;

        instruction_pointer = operand_addr; 
    }

    void bgz()
    {
        std::cout << "EXECUTING BRANCHNEG" << std::endl;

        if (accumulator > 0) {
            instruction_pointer = operand_addr;
        }
    }


    void bez()
    {
        std::cout << "EXECUTING BRANCHZERO" << std::endl;

        if (accumulator == 0) {
            instruction_pointer = operand_addr;
        }
    }

    void sor()
    {
        std::cout << "EXECUTING OR: " << accumulator << " - " << operand_data << std::endl;
        accumulator |= static_cast<unsigned short>(data);
    }

    void sand()
    {
        std::cout << "EXECUTING OR: " << accumulator << " - " << operand_data << std::endl;
        accumulator &= static_cast<unsigned short>(data);
    }



    void execute_op()
    {
        switch(opcode)
        {
            case SOR:
                sor();
                break;
            case SAND:
                sand();
                break;
            case LOAD:
                load();
                break;
            case STORE:
                store();
                break;
            case ADD:
                add();
                break;
            case SUB:
                sub();
                break;
            case DIV: 
                div();
                break;
            case MUL: 
                mul();
                break;
            case JMP:
                jmp();
                break;
            case BGZ:
                bgz();
                break;
            case BEZ:
                bez();
                break;
            case SOR:
                sor();
                break;
            case SAND:
                sand();
                break;
            case HALT:
                halt();
                break;
        }
        
    }

    void set_defaults()
    {
        i_we = false;
    }

    void print_internals()
    {
        std::cout << "Address: " << std::hex << i_address << std::endl;
        std::cout << "Data: " << std::hex << i_data << std::endl;
        std::cout << "we: " << std::hex << i_we << std::endl;
    }

    void assign_internals(){
        i_address = address;
        i_data = data;
        i_we = we;
    }

    void assign_signals(){
        address = i_address;
        data = i_data;
        we = i_we;
    }
   
    void run()
    {
        unsigned short cycle = 0;
        
        while(true){
           
            std::cout << "---------------------------------------------------------------------" << std::endl;
            std::cout << "Iteration: " << cycle++ << std::endl;

        
            if (!initialized){

                address = instruction_pointer;
                set_defaults();
                initialized = true;
            } 
            else if (executing){
                switch(state)
                {
                   case FETCH:
                        /* State Enter */
                        std::cout << "[State FETCH <-]" << std::endl;
                        assign_internals();
                        print_internals();

                        /* Set state outputs */
                        set_defaults();
                        i_address = instruction_pointer;
                        i_we = false;

                        /* Update internal registers */
                        instruction_pointer++;

                        /* State Leave */
                        std::cout << "[State FETCH ->]" << std::endl;
                        assign_signals();
                        print_internals();

                        /* Next state */
                        state = DECODE;

                        break;

                   case DECODE:
                        
                        /* State Enter */
                        std::cout << "[State DECODE <-]" << std::endl;
                        assign_internals();
                        print_internals();

                        /* Update internal registers */
                        opcode = (data & 0xF000) >> 12;
                        operand_addr = data & 0x0FFF;
                        std::cout << "OPCODE: " << opcode << " " << "OPERAND: " << operand_addr << std::endl;
                        
                        /* Set state outputs */
                        set_defaults();

                        if (opcode == LOAD || opcode == ADD || opcode == SUB
                         || opcode == DIV  || opcode == MUL || opcode == WRITE
                         )
                        {
                            i_address = operand_addr;
                        }


                        /* State Leave */
                        std::cout << "[State DECODE ->]" << std::endl;
                        assign_signals();
                        print_internals();

                        /* Next state */
                        state = EXEC;

                        break;
                   case EXEC:

                        /* State Enter */
                        std::cout << "[State EXEC <-]" << std::endl;
                        assign_internals();
                        print_internals();

                        operand_data = data;

                        /* Execute instruction */
                        set_defaults();
                        execute_op();

                        /* State Leave */
                        std::cout << "[State EXEC ->]" << std::endl;
                        assign_signals();
                        print_internals();

                        /* Next state */
                        state = FETCH;

                        break;
                    case WRITE_BACK:
                        std::cout << "WRITE_BACK" << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        we = true;
                        state = FETCH;
                }
            }
            

            std::cout << sc_time_stamp() << std::endl;
            wait();
        }
    }
    
    SC_CTOR(Simpletron)
    {
        SC_THREAD(run);
        sensitive << clk.pos(); 
    }

};
