section .text
global _start

_start:
  mov RCX, 5
  mov RDX, 5
  add RCX, RDX
  mov RDX, 10
  add RCX, RDX
  mov RAX, 60
  mov RDI, RCX
  syscall
