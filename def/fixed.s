; This assembly is always included into the build.

; eax is the first address.
; ebx is the size in dwords.
; Both registers are modified.
global memclear;
memclear:
  add eax, 8;
  shl ebx, 2;
  add ebx, eax;
  memclear_loop:
  cmp eax, ebx;
  je memclear_return;
  mov dword [eax], 0;
  add eax, 4;
  jmp memclear_loop;
  memclear_return:
  ret;

; check whether value in eax is Null/0
global nullcheck;
nullcheck:
  cmp eax, 0;
  extern __exception;
  je __exception;
  ret;
