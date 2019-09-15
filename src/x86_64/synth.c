#include <runtime.h>
#include <synth.h>

#define modrm_register_mode 3
#define toboolean(__x) ((__x)?true:false)

void sib(buffer b, u32 scale, u32 index, u32 base)
{
    push_u8(b, scale<<6 | index<<3 | base);
}

void debug_trap(buffer b)
{
    push_u8(b, 0xcc);
}

void modrm(buffer b, u32 mod, u32 reg, u32 rm)
{
    push_u8(b, mod<<6 | reg<<3 | rm);
}

void rex(buffer b, boolean width, boolean reg, boolean index, boolean base)
{
    push_u8(b, 0x40 | (width<<3) | (reg <<2) | (index <<1) | base);
}

void regop(buffer b, u64 op, u64 modr, reg r)
{
    int extended = (r&8)?1:0;
    if (extended) rex(b, false, false, false, true);
    push_u8(b, op);
    modrm(b, modrm_register_mode, modr, r&7);
}

void move_64_imm(buffer b, reg r, u64 imm)
{
    rex(b, true, false, false, false);
    push_u8(b, 0xb8 + r);
    buffer_write_le64(b, imm);
}

void move_32_imm(buffer b, reg r, u32 imm)
{
    //    rex(b, true, false, false, false);
    push_u8(b, 0xb8 + r);
    buffer_write_le32(b, imm);
}

void indirect(buffer b, reg dest, reg source)
{
    rex(b, true, false, false, false);
    push_u8(b, 0x8b);
    modrm(b, 0, dest, source);    
}

void jump_indirect(buffer b, reg r)
{
    push_u8(b, 0xff);
    // nasm synthesizes 0x20 for modrm at some point, and objdump
    // dissembles it correctly, but doesn't execute..use modrm function
    push_u8(b, 0xe0 + r);    
}

void indirect_displacement(buffer b, reg dest, reg source, u32 d)
{
    rex(b, 1, 0, 0, 0);
    push_u8(b, 0x8b);
    rprintf ("kil %d\n", d);
    // mode 1 is single trailing byte of displacment
    // signed? I guesso
    if (d < 128) {
        modrm(b, 1, dest, source);
        push_u8(b, d);        
    } else {
        // mode 2 is 32 bit word of displacment        
        modrm(b, 2, dest, source);
        buffer_write_le32(b, d);        
    }
}

void move_register(buffer b,reg from, reg to)
{
    // rex op modrm 
    rex(b, true, toboolean(from & 0x8), 0, toboolean(to & 0x8));
    push_u8(b,0x8b); 
    modrm (b, 3, from&7, to&7);
}

void call_register(buffer b, reg r)
{
    regop(b, 0xff, 2, r);
}

void push_register(buffer b, reg to)
{
    // shorter 0x50 + r&7
    return(regop(b, 0xff, 6, to));
}

#define SIB_FOLLOWS_REGISTER 0x4
// d = a + b * 2^s
void indirect_scale(buffer b, reg dest, u32 scale, reg index, reg base)
{
    rex(b, 1, 0, 0, 0);
    push_u8(b, 0x8b);
    modrm(b, 0, dest, SIB_FOLLOWS_REGISTER);
    sib(b, scale, index, base);
}

void returnop(buffer b)
{
    push_u8(b, 0xc3);
}
