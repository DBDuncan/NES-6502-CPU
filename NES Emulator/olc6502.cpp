#include "olc6502.h"
#include "bus.h"
#include <sstream>

olc6502::olc6502()
{

	using a = olc6502;

	lookup =
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
	};

}

olc6502::~olc6502()
{


}

uint8_t olc6502::read(uint16_t a)
{
	return bus->read(a, false);
}

void olc6502::write(uint16_t a, uint8_t d)
{
	bus->write(a, d);
}


void olc6502::clock()
{
	if (cycles == 0)
	{
		//read opcode and increment pc
		opcode = read(pc);

		SetFlag(U, true);


		pc++;

		//get number of cycles from lookup
		cycles = lookup[opcode].cycles;

		//calls address mode function
		uint8_t additional_cycle1 = (this->*lookup[opcode].addrmode)();

		//calls operate function for spercific instruction
		uint8_t additional_cycle2 = (this->*lookup[opcode].operate)();

		//if both indicate they need an aditional clock cycle, then add that clock cycle
		cycles += (additional_cycle1 & additional_cycle1);
		SetFlag(U, true);
	}
	clock_count++;
	cycles--;
}

void olc6502::SetFlag(FLAGS6502 f, bool v)
{
	if (v)
		status |= f;
	else
		status &= ~f;
}

uint8_t olc6502::GetFlag(FLAGS6502 f)
{
	return ((status & f) > 0) ? 1 : 0;
}


// addressing modes

//implied
//not doing anything with data, but could be operating on the accumilator
uint8_t olc6502::IMP()
{
	//set fetched variable to content of accumilator
	fetched = a;
	return 0;
}

//immediate
//read the next byte as a value, so set addr_abs to the location of the byte to read
uint8_t olc6502::IMM()
{
	addr_abs = pc++;
	return 0;
}

//zero page
//way of structuring memory, 256 pages of 256 bytes
//this function reads the low byte of the zero page

//zero page addressing means the byte being searched for can be found somewhere in page zero
//means less memory can be used for code
uint8_t olc6502::ZP0()
{
	addr_abs = read(pc);
	pc++;
	//erase the high bytes of the variable to only read page zero
	addr_abs &= 0x00FF;
	return 0;
}

//zero page with z offset
//good for iterating through memory

//same as ZP0, but the content of the x register is added to the memory location to be read
uint8_t olc6502::ZPX()
{
	//read pc location plus x offset
	addr_abs = (read(pc) + x);
	pc++;
	addr_abs &= 0x00FF;
	return 0;
}

//zero page with y offset
//same as above but uses y register
uint8_t olc6502::ZPY()
{
	addr_abs = (read(pc) + y);
	pc++;
	addr_abs &= 0x00FF;
	return 0;
}


//relative
//used for jumping and is exclusive to the branch instructions
//address must be within -128 to +127 of the branch instruction for jump
//meaning your cant directly branch to any address outside of the range.
uint8_t olc6502::REL()
{
	//address stored in seperate varable beacuse it is relitive instead of absolute like all the others
	addr_rel = read(pc);
	pc++;
	//checking if signed if bit seven is set to one
	if (addr_rel & 0x80)
		//then set the high bytes to all ones
		addr_rel |= 0xFF00;
	return 0;
}

//absolute
//reads memory location using full 16bit address
uint8_t olc6502::ABS()
{
	uint16_t lo = read(pc);
	pc++;
	uint16_t hi = read(pc);
	pc++;

	//or the two variables together to get a 16 bit address word
	addr_abs = (hi << 8) | lo;

	return 0;
}

//absolute with x offset
//is basicly ABS and ZPX mushed together. 
//reads a 16 bit address and adds content of x register
uint8_t olc6502::ABX()
{
	//read address
	uint16_t lo = read(pc);
	pc++;
	uint16_t hi = read(pc);
	pc++;
	//and offset it by the contents of the x register
	addr_abs = (hi << 8) | lo;
	addr_abs += x;

	//but, if address has gone to another page, an additional clock cycle may be needed
	//checked by seeing if the high byte (page byte) has changed after adding content of register
	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;

}

//absolute with y offset
//is basicly ABS and ZPX mushed together. 
//reads a 16 bit address and adds content of y register
uint8_t olc6502::ABY()
{
	//read address
	uint16_t lo = read(pc);
	pc++;
	uint16_t hi = read(pc);
	pc++;
	//and offset it by the contents of the y register
	addr_abs = (hi << 8) | lo;
	addr_abs += y;

	//but, if address has gone to another page, an additional clock cycle may be needed
	//checked by seeing if the high byte (page byte) has changed after adding content of register
	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;

}

//indirect indressing incoming

//indirect
//the suppllied address with the function is a pointer. get the pointer
//to get the address of the data desired

//gets the address that the pointer points to
uint8_t olc6502::IND()
{
	uint16_t ptr_lo = read(pc);
	pc++;
	uint16_t ptr_hi = read(pc);
	pc++;

	uint16_t ptr = (ptr_hi << 8) | ptr_lo;

	//this simulates page boundry hardware bug if the low byte is 0xFF
	//prob is meant to wrap to the next page, but this bug just makes it
	//fetch the data from xx00 instead of page xx+1
	if (ptr_lo == 0x00FF)
	{
		addr_abs = (read(ptr & 0x00FF) << 8) | read(ptr + 0);
	}
	else//else behave normally
	{
		addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
	}

	return 0;
}

//indirect x
//indirect addressing of the zero page with x offset
uint8_t olc6502::IZX()
{
	uint16_t t = read(pc);
	pc++;

	uint16_t lo = read((uint16_t)(t + (uint16_t)x) & 0x00FF);
	uint16_t hi = read((uint16_t)(t + (uint16_t)x + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;

	return 0;
}

//indirect y
//indirect addressing of the zero page with y offset
//for some reason different from IZX??? will look into this
//something with crossing a page boundry
uint8_t olc6502::IZY()
{
	uint16_t t = read(pc);
	pc++;

	uint16_t lo = read(t & 0x00FF);
	uint16_t hi = read((t + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;
	addr_abs += y;

	//again, by adding to addr_abs directly, may cross page boundry,
	//so have to signal because would take more cycles
	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;
}




//instructions

//return data for all instructions except those using implied address mode
//because there is nothing to fetch
uint8_t olc6502::fetch()
{
	if (!(lookup[opcode].addrmode == &olc6502::IMP))
		fetched = read(addr_abs);
	return fetched;
}

uint8_t olc6502::AND()
{
	fetch();

	//perform operation, and accuulator register and fetched value from memory
	a = a & fetched;

	//if all bits are zero, set zero flag
	SetFlag(Z, a == 0x00);
	//set negative flag if bit seven is set to one
	SetFlag(N, a & 0x80);

	//and potentilly uses dditional clock cycles
	return 1;
}


//branch if carry bit is set
uint8_t olc6502::BCS()
{
	if (GetFlag(C) == 1)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if carry clear
uint8_t olc6502::BCC()
{
	if (GetFlag(C) == 0)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if equal
uint8_t olc6502::BEQ()
{
	if (GetFlag(Z) == 1)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if negitive
uint8_t olc6502::BMI()
{
	if (GetFlag(N) == 1)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if not equal
uint8_t olc6502::BNE()
{
	if (GetFlag(Z) == 0)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if positive
uint8_t olc6502::BPL()
{
	if (GetFlag(N) == 0)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if overflow
uint8_t olc6502::BVC()
{
	if (GetFlag(V) == 0)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}

//branch if not overflow
uint8_t olc6502::BVS()
{
	if (GetFlag(V) == 1)
	{
		//when branch is taken add to cycles
		cycles++;
		addr_abs = pc + addr_rel;

		//if branch crosses page boundry, add to number of cycles
		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}

	return 0;
}


//clear carry flag
uint8_t olc6502::CLC()
{
	SetFlag(C, false);
	return 0;
}

//clear decimal flag
uint8_t olc6502::CLD()
{
	SetFlag(D, false);
	return 0;
}

//clear interupt flag
uint8_t olc6502::CLI()
{
	SetFlag(I, false);
	return 0;
}

//clear overflow flag
uint8_t olc6502::CLV()
{
	SetFlag(V, false);
	return 0;
}

//add
uint8_t olc6502::ADC()
{
	fetch();

	//if decimal mode
	if (GetFlag(D) == 1)
	{
		//temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);




		//seperate 4 bits into two seperate variables.
		//add them together, timesing by 10 the byte from the high end
		//do the same for the other variable and add them together
		//then see if overflowed

		uint16_t fetchedLo = (uint16_t)fetched & 0x000F;
		uint16_t fetchedHi = (uint16_t)fetched >> 4;

		uint16_t fetchedTotal = (fetchedHi * 10) + fetchedLo;


		uint16_t accLo = (uint16_t)a & 0x000F;
		uint16_t accHi = (uint16_t)a >> 4;

		uint16_t accTotal = (accHi * 10) + accLo;


		temp = fetchedTotal + accTotal;



		if (GetFlag(C) == 1)
		{
			temp += 1;
		}


		if (temp > 99)
		{

			temp = temp - 100;

			SetFlag(C, true);

		}
		else
		{
			SetFlag(C, false);
		}



		//need to turn int to hex, not math equivilent i.e. 55 to 0x55 not 0x37

		
		uint16_t endChar = temp % 10;
		uint16_t firstChar = temp / 10;
		//uint16_t second = temp;


		//code not correct here
		//fixed now


		uint16_t finalInt = (firstChar << 4) | endChar;


		//set zero flag
		SetFlag(Z, ((finalInt & 0x00FF) == 0));
		//set neg flag
		SetFlag(N, temp & 0x0080);

		//not bothering with the v flag, i believe it is undocumented in decimal mode 



		//SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);



		//only keeping the two end bytes
		a = finalInt & 0x00FF;



		//set zero flag
		//SetFlag(Z, (a & 0x00FF) == 0);
		//set neg flag
		//SetFlag(N, (a > 127));







	}
	else//if not decimal mode
	{
		//add a and fetched data plus 1 if carry flag is set
		//represented in 16 bit form for carry reasons
		temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);
		//set carry flag
		SetFlag(C, temp > 255);
		//set zero flag
		SetFlag(Z, (temp & 0x00FF) == 0);
		//set neg flag if neg bit is set
		SetFlag(N, temp & 0x80);
		//set overflow flag (complicated part)
		SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
		a = temp & 0x00FF;

	}
	return 1;
}

//subtract
uint8_t olc6502::SBC()
{
	fetch();



	if (GetFlag(D) == 1)
	{


		uint16_t fetchedLo = (uint16_t)fetched & 0x000F;
		uint16_t fetchedHi = (uint16_t)fetched >> 4;

		uint16_t fetchedTotal = (fetchedHi * 10) + fetchedLo;


		uint16_t accLo = (uint16_t)a & 0x000F;
		uint16_t accHi = (uint16_t)a >> 4;

		uint16_t accTotal = (accHi * 10) + accLo;


		int16_t tempHold = (int16_t)accTotal - (int16_t)fetchedTotal;


		//for some reason takes away one when carry is clear???
		if (GetFlag(C) == 0)
		{
			tempHold -= 1;
		}



		if (tempHold < 0)
		{

			tempHold = 100 + tempHold;

			SetFlag(C, false);

		}
		else
		{
			SetFlag(C, true);
		}

		temp = (uint16_t)tempHold;


		//need to turn int to hex, not math equivilent i.e. 55 to 0x55 not 0x37


		uint16_t endChar = temp % 10;
		uint16_t firstChar = temp / 10;
		//uint16_t second = temp;


		//code not correct here


		uint16_t finalInt = (firstChar << 4) | endChar;


		//set zero flag
		SetFlag(Z, ((finalInt & 0x00FF) == 0));

		//set neg flag
		SetFlag(N, temp & 0x0080);


		//SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ finalInt) & 0x0080);




		//only keeping the two end bytes
		a = finalInt & 0x00FF;



	}
	else
	{

		//get and invert data using exclusive or function
		uint16_t value = ((uint16_t)fetched) ^ 0x00FF;

		//notice how code is same as addition (ADC) from here
		temp = (uint16_t)a + value + (uint16_t)GetFlag(C);
		SetFlag(C, temp & 0xFF00);
		SetFlag(Z, ((temp & 0x00FF) == 0));
		SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
		SetFlag(N, temp & 0x0080);
		a = temp & 0x00FF;

	}

	return 1;
}


//pushes accumilator to stack.
//the stack exists in memory
uint8_t olc6502::PHA()
{
	//0x0100 is base location of stack pointer
	write(0x0100 + stkp, a);
	stkp--;
	return 0;
}

//read from stack
//pop accumulator off stack
uint8_t olc6502::PLA()
{
	stkp++;
	a = read(0x0100 + stkp);
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}


//sets the CPU to a known state
void olc6502::reset()
{

	a = 0;
	x = 0;
	y = 0;
	stkp = 0xFD;
	status = 0x00 | U;

	//read this location to find location to start executing code
	addr_abs = 0xFFFC;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);

	pc = (hi << 8) | lo;

	addr_rel = 0x0000;
	addr_abs = 0x0000;
	fetched = 0x00;

	cycles = 8;

}


void olc6502::irq()
{
	if (GetFlag(I) == 0)
	{
		//write current program counter to stack
		//takes two writes because the program counter is 16 bit
		write(0x0100 + stkp, (pc >> 8) & 0x00FF);
		stkp--;
		write(0x0100 + stkp, pc & 0x00FF);
		stkp--;

		//write status registers to stack
		SetFlag(B, 0);
		SetFlag(U, 1);
		SetFlag(I, 1);
		write(0x0100 + stkp, status);
		stkp--;


		addr_abs = 0xFFFE;
		uint16_t lo = read(addr_abs + 0);
		uint16_t hi = read(addr_abs + 1);
		pc = (hi << 8) | lo;

		cycles = 7;
	}
}


//same as irq, but nothing can stop it
void olc6502::nmi()
{
	//write current program counter to stack
	//takes two writes because the program counter is 16 bit
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	//write status registers to stack
	SetFlag(B, 0);
	SetFlag(U, 1);
	SetFlag(I, 1);
	write(0x0100 + stkp, status);
	stkp--;


	addr_abs = 0xFFFA;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);
	pc = (hi << 8) | lo;

	cycles = 8;
}



//returns cpu to state before interupt occured
uint8_t olc6502::RTI()
{

	stkp++;
	status = read(0x0100 + stkp);
	status &= ~B;
	status &= ~U;

	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;

	return 0;
}


//shift left
uint8_t olc6502::ASL()
{
	fetch();
	temp = (uint16_t)fetched << 1;
	//set if overflows above 255
	SetFlag(C, (temp & 0xFF00) > 0);
	//set if zero
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	//set if negative
	SetFlag(N, temp & 0x80);

	if (lookup[opcode].addrmode == &olc6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;


}



uint8_t olc6502::BIT()
{
	fetch();
	temp = a & fetched;
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	SetFlag(N, fetched & (1 << 7));
	SetFlag(V, fetched & (1 << 6));

	return 0;
}


//break
uint8_t olc6502::BRK()
{
	pc++;

	SetFlag(I, 1);
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	SetFlag(B, 1);
	write(0x0100 + stkp, status);
	stkp--;
	SetFlag(B, 0);

	pc = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);

	return 0;


}


//compatre accumulator
uint8_t olc6502::CMP()
{
	fetch();
	temp = (uint16_t)a - (uint16_t)fetched;
	//if a > then fetched then overflow
	SetFlag(C, a >= fetched);
	//check if zero
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	//check if negative
	SetFlag(N, temp & 0x0080);
	return 1;
}


//compare x register
uint8_t olc6502::CPX()
{
	fetch();
	temp = (uint16_t)x - (uint16_t)fetched;
	SetFlag(C, x >= fetched);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}


//compare y register
uint8_t olc6502::CPY()
{
	fetch();
	temp = (uint16_t)y - (uint16_t)fetched;
	SetFlag(C, y >= fetched);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}



//decrement x register
uint8_t olc6502::DEX()
{
	x--;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


//decrement y register
uint8_t olc6502::DEY()
{
	y--;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}


//bitwise logic XOR
uint8_t olc6502::EOR()
{
	fetch();
	a = a ^ fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}


//increment value at memory location
uint8_t olc6502::INC()
{
	fetch();
	temp = fetched + 1;
	write(addr_abs, temp & 0x00FF);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}



//increment x register
uint8_t olc6502::INX()
{
	x++;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


//increment y register
uint8_t olc6502::INY()
{
	y++;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}



//jump to location
uint8_t olc6502::JMP()
{
	pc = addr_abs;
	return 0;
}


//jump to sub routine
uint8_t olc6502::JSR()
{
	pc--;
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	pc = addr_abs;
	return 0;
}


//load accumilator
uint8_t olc6502::LDA()
{
	fetch();
	a = fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}

//load x register
uint8_t olc6502::LDX()
{
	fetch();
	x = fetched;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 1;
}


//load y register
uint8_t olc6502::LDY()
{
	fetch();
	y = fetched;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 1;
}



uint8_t olc6502::LSR()
{
	fetch();
	SetFlag(C, fetched & 0x0001);
	temp = fetched >> 1;
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &olc6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}


//handles illegal opcodes
uint8_t olc6502::NOP()
{
	// Sadly not all NOPs are equal, Ive added a few here
	// based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
	// and will add more based on game compatibility, and ultimately
	// I'd like to cover all illegal opcodes too
	switch (opcode) {
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1;
		break;
	}
	return 0;
}



//bitwise logical OR
uint8_t olc6502::ORA()
{
	fetch();
	a = a | fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}

/*
//push accumulator to stack
uint8_t olc6502::PHA()
{
	write(0x0100 + stkp, a);
	stkp--;
	return 0;
}
*/


//push register to stack
uint8_t olc6502::PHP()
{
	write(0x0100 + stkp, status | B | U);
	SetFlag(B, 0);
	SetFlag(U, 0);
	stkp--;
	return 0;
}

/*
//pop accumulator off stack
uint8_t olc6502::PLA()
{
	stkp++;
	a = read(0x0100 + stkp);
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}
*/



//pop status register off stack
uint8_t olc6502::PLP()
{
	stkp++;
	status = read(0x0100 + stkp);
	SetFlag(U, 1);
	return 0;
}



uint8_t olc6502::ROL()
{
	fetch();
	temp = (uint16_t)(fetched << 1) | GetFlag(C);
	SetFlag(C, temp & 0xFF00);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &olc6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}

uint8_t olc6502::ROR()
{
	fetch();
	temp = (uint16_t)(GetFlag(C) << 7) | (fetched >> 1);
	SetFlag(C, fetched & 0x01);
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &olc6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}

/*
uint8_t olc6502::RTI()
{
	stkp++;
	status = read(0x0100 + stkp);
	status &= ~B;
	status &= ~U;

	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;
	return 0;
}
*/

uint8_t olc6502::RTS()
{
	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;

	pc++;
	return 0;
}


//set carry flag
uint8_t olc6502::SEC()
{
	SetFlag(C, true);
	return 0;
}


//set decimal flag
uint8_t olc6502::SED()
{
	SetFlag(D, true);
	return 0;
}


//set interrupt flag / enable interrupts
uint8_t olc6502::SEI()
{
	SetFlag(I, true);
	return 0;
}


//store accumulator at address
uint8_t olc6502::STA()
{
	write(addr_abs, a);
	return 0;
}


//store x register at address
uint8_t olc6502::STX()
{
	write(addr_abs, x);
	return 0;
}


//store y register at address
uint8_t olc6502::STY()
{
	write(addr_abs, y);
	return 0;
}


//transfer accumulator to x register
uint8_t olc6502::TAX()
{
	x = a;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


//transfer accumulator to y register
uint8_t olc6502::TAY()
{
	y = a;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}




//transfer stack pointer to x register
uint8_t olc6502::TSX()
{
	x = stkp;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


//transfer x register to accumulator
uint8_t olc6502::TXA()
{
	a = x;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}


//transfer x register to stack pointer
uint8_t olc6502::TXS()
{
	stkp = x;
	return 0;
}


//transfer y register to accumulator
uint8_t olc6502::TYA()
{
	a = y;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}

//decrement value at memory location
uint8_t olc6502::DEC()
{
	fetch();
	temp = fetched - 1;
	write(addr_abs, temp & 0x00FF);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}



//catch illegal opcodes
uint8_t olc6502::XXX()
{
	return 0;
}


// helper functions


bool olc6502::complete()
{
	return cycles == 0;
}



//turns binary instruction code into readable form.
std::map<uint16_t, std::string> olc6502::disassemble(uint16_t nStart, uint16_t nStop)
{
	uint32_t addr = nStart;
	uint8_t value = 0x00, lo = 0x00, hi = 0x00;
	std::map<uint16_t, std::string> mapLines;
	uint16_t line_addr = 0;

	// A convenient utility to convert variables into
	// hex strings because "modern C++"'s method with 
	// streams is atrocious
	auto hex = [](uint32_t n, uint8_t d)
	{
		std::string s(d, '0');
		for (int i = d - 1; i >= 0; i--, n >>= 4)
			s[i] = "0123456789ABCDEF"[n & 0xF];
		return s;
	};

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	while (addr <= (uint32_t)nStop)
	{
		line_addr = addr;

		// Prefix line with instruction address
		std::string sInst = "$" + hex(addr, 4) + ": ";

		// Read instruction, and get its readable name
		uint8_t opcode = bus->read(addr, true); addr++;
		sInst += lookup[opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (lookup[opcode].addrmode == &olc6502::IMP)
		{
			sInst += " {IMP}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IMM)
		{
			value = bus->read(addr, true); addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZP0)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZPX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ZPY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IZX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IZY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABS)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABX)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (lookup[opcode].addrmode == &olc6502::ABY)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (lookup[opcode].addrmode == &olc6502::IND)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (lookup[opcode].addrmode == &olc6502::REL)
		{
			value = bus->read(addr, true); addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}