# 0 "bootasm.S"
# 1 "/home/dev/src/xv6/x86/xv6-public//"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "bootasm.S"
# 1 "asm.h" 1
# 2 "bootasm.S" 2
# 1 "memlayout.h" 1
# 3 "bootasm.S" 2
# 1 "mmu.h" 1
# 4 "bootasm.S" 2

# Start the first CPU: switch to 32-bit protected mode, jump into C.
# The BIOS loads this code from the first sector of the hard disk into
# memory at physical address 0x7c00 and starts executing in real mode
# with %cs=0 %ip=7c00.

.code16 # Assemble for 16-bit mode
.globl start
start:
  cli # BIOS enabled interrupts; disable

  # Zero data segment registers DS, ES, and SS.
  xorw %ax,%ax # Set %ax to zero
  movw %ax,%ds # -> Data Segment
  movw %ax,%es # -> Extra Segment
  movw %ax,%ss # -> Stack Segment

  # Physical address line A20 is tied to zero so that the first PCs
  # with 2 MB would run software that assumed 1 MB. Undo that.
seta20.1:
  inb $0x64,%al # Wait for not busy
  testb $0x2,%al
  jnz seta20.1

  movb $0xd1,%al # 0xd1 -> port 0x64
  outb %al,$0x64

seta20.2:
  inb $0x64,%al # Wait for not busy
  testb $0x2,%al
  jnz seta20.2

  movb $0xdf,%al # 0xdf -> port 0x60
  outb %al,$0x60

  # Switch from real to protected mode. Use a bootstrap GDT that makes
  # virtual addresses map directly to physical addresses so that the
  # effective memory map doesn't change during the transition.
  lgdt gdtdesc
  movl %cr0, %eax
  orl $0x00000001, %eax
  movl %eax, %cr0


  # Complete the transition to 32-bit protected mode by using a long jmp
  # to reload %cs and %eip. The segment descriptors are set up with no
  # translation, so that the mapping is still the identity mapping.
  ljmp $(1<<3), $start32

.code32 # Tell assembler to generate 32-bit code now.
start32:
  # Set up the protected-mode data segment registers
  movw $(2<<3), %ax # Our data segment selector
  movw %ax, %ds # -> DS: Data Segment
  movw %ax, %es # -> ES: Extra Segment
  movw %ax, %ss # -> SS: Stack Segment
  movw $0, %ax # Zero segments not ready for use
  movw %ax, %fs # -> FS
  movw %ax, %gs # -> GS

  # Set up the stack pointer and call into C.
  movl $start, %esp
  call bootmain

  # If bootmain returns (it shouldn't), trigger a Bochs
  # breakpoint if running under Bochs, then loop.
  movw $0x8a00, %ax # 0x8a00 -> port 0x8a00
  movw %ax, %dx
  outw %ax, %dx
  movw $0x8ae0, %ax # 0x8ae0 -> port 0x8a00
  outw %ax, %dx
spin:
  jmp spin

# Bootstrap GDT
.p2align 2 # force 4 byte alignment
gdt:
  .word 0, 0; .byte 0, 0, 0, 0 # null seg
  .word (((0xffffffff) >> 12) & 0xffff), ((0x0) & 0xffff); .byte (((0x0) >> 16) & 0xff), (0x90 | (0x8|0x2)), (0xC0 | (((0xffffffff) >> 28) & 0xf)), (((0x0) >> 24) & 0xff) # code seg
  .word (((0xffffffff) >> 12) & 0xffff), ((0x0) & 0xffff); .byte (((0x0) >> 16) & 0xff), (0x90 | (0x2)), (0xC0 | (((0xffffffff) >> 28) & 0xf)), (((0x0) >> 24) & 0xff) # data seg

gdtdesc:
  .word (gdtdesc - gdt - 1) # sizeof(gdt) - 1
  .long gdt # address gdt
