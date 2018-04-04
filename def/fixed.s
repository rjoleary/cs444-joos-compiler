; This assembly is always included into the build.

; eax is the first address.
; ebx is the size in dwords.
; Both registers are modified.
global memclear;
memclear:
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

; Perform an instance of lookup (eax instanceof ebx).
; eax is a pointer to the source object.
; ebx is a pointer to the target vtable.
; Returns:
;   1: when eax can be dynamically cast to ebx
;   0: otherwise
global instanceOfLookup;
instanceOfLookup:
  cmp eax, 0;
  je instanceOfReturnFalse;
  mov eax, [eax];
  cmp eax, 0;
  je instanceOfReturnFalse;
  instanceOfLookupLoop:
    sub eax, 4;
    mov ecx, [eax];
    cmp ecx, 0;
    je instanceOfReturnFalse;
    cmp ecx, ebx;
    je instanceOfReturnTrue;
    jmp instanceOfLookupLoop;
  instanceOfReturnTrue:
    mov eax, 1;
    ret;
  instanceOfReturnFalse:
    mov eax, 0;
    ret;
