SC_MODULE(Simpletron)
{
    sc_in<bool> clk;
    sc_out<int> address;
    sc_inout<int> data;
    sc_out<bool> ram_rw; // WE = 0 -> Read, WE = 1 -> Write
    sc_out<int> acc_debug;

    int i_address = 0;
    int i_data = 0;
    bool i_ram_rw = false;

    enum State {FETCH, DECODE, EXEC, WRITE_BACK};
    State state = FETCH;
    
    enum OpCodes {READ      = 0x01,
                  WRITE     = 0x02,
                  LOAD      = 0x03,
                  STORE     = 0x04,
                  ADD       = 0x05,
                  SUB       = 0x06,
                  DIV       = 0x07,
                  MUL       = 0x08,
                  JMP       = 0x09,
                  BLZ       = 0x0A,
                  BEZ       = 0x0B,
                  HALT      = 0x0C
                 };

    int accumulator = 0;
    int fetch_pointer = 0;
    bool initialized = false;
    bool executing = true;
    int instruction_pointer = 0;

    int opcode;
    int operand;


    void load()
    {
        std::cout << "EXECUTING LOAD" << std::endl;
        accumulator = data;
    }
    
    void store()
    {
        std::cout << "EXECUTING STORE" << std::endl;
        i_data = accumulator;
        i_ram_rw = true;
    }
    void halt()
    {
        std::cout << "EXECUTING HALT" << std::endl;
        int stop;
        executing = false; 
        std::cin >> stop;
    }


    void add()
    {
        std::cout << "EXECUTING ADD" << std::endl;
        accumulator += data; 
    }


    void sub()
    {
        std::cout << "EXECUTING SUB: " << accumulator << " - " << data << std::endl;
        accumulator -= static_cast<int>(data);
    }


    void div()
    {
        std::cout << "EXECUTING DIV" << std::endl;

        if (data != 0){
            accumulator /= data;
        } else {
            std::cout << "Error: Division by zero"; 
            halt();
        }
    }


    void mul()
    {
        std::cout << "EXECUTING MUL" << std::endl;
        accumulator *= data;
    }

    void jmp()
    {
        std::cout << "EXECUTING BRANCH" << std::endl;

        instruction_pointer = address; 
    }


    void blz()
    {
        std::cout << "EXECUTING BRANCHNEG" << std::endl;

        if (accumulator < 0) {
            instruction_pointer = address;
        }
    }


    void bez()
    {
        std::cout << "EXECUTING BRANCHZERO" << std::endl;

        if (accumulator == 0) {
            instruction_pointer = address;
        }
    }

    void write()
    {
        std::cout << "EXECUTING WRITE" << std::endl;
        std::cout << "### CONSOLE: " << std::hex << data << std::endl;
    }


    void read()
    { 
        int data_out;
        std::cout << "EXECUTING READ" << std::endl;
        std::cout << " ### CONSOLE: ";
        std::cin >> data_out;
        i_ram_rw = true;
        i_data = data_out;
    }



    void execute_op()
    {
        switch(opcode)
        {
            case READ:
                read();
                break;
            case WRITE:
                write();
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
            case BLZ:
                blz();
                break;
            case BEZ:
                bez();
                break;
            case HALT:
                halt();
                break;

        }
        
    }

    void set_defaults()
    {
        i_ram_rw = false;
    }
    void print_internals()
    {
        std::cout << "Address: " << std::hex << i_address << std::endl;
        std::cout << "Data: " << std::hex << i_data << std::endl;
        std::cout << "Ram_RW: " << std::hex << i_ram_rw << std::endl;
    }

    void assign_internals(){
        i_address = address;
        i_data = data;
        i_ram_rw = ram_rw;
    }

    void assign_signals(){
        address = i_address;
        data = i_data;
        ram_rw = i_ram_rw;
    }
   
    void run()
    {
        int cycle = 0;
        
        while(true){
           
            std::cout << "---------------------------------------------------------------------" << std::endl;
            std::cout << "Iteration: " << cycle++ << std::endl;
        
            if (!initialized){

                address = instruction_pointer;
                set_defaults();
                acc_debug = accumulator;
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
                        i_ram_rw = false;

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
                        operand = data & 0x0FFF;
                        std::cout << "OPCODE: " << opcode << " " << "OPERAND: " << operand << std::endl;
                        
                        /* Set state outputs */
                        set_defaults();

                        i_address = operand;

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
                        ram_rw = true;
                        state = FETCH;
                }
                acc_debug = accumulator;
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
