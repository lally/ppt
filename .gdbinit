layout asm

define n
nexti
printf "eax=%x ebx=%x ecx=%x edx=%x esi=%x edi=%x\n",$eax,$ebx,$ecx,$edx,$esi,$edi
end
define stride
print *(int *)(0x3c +$esp)
end

define count
print *(int *)(0x38 +$esp)
end

define seqno
print *(int *)(0x34 +$esp)
end

define last_seqno
print *(int *)(0x30 +$esp)
end

define delay
print *(int *)(0x2c +$esp)
end

define cont_read
print *(int *)(0x24 +$esp)
end

define p_cur
printf "p_cur=%x, *p_cur=%d\n",*(int**)(0x24+$esp),**(int**)(0x24+$esp)
end

define p_stride
printf "p_stride=%x, *p_stride=%d\n",*(int**)(0x20+$esp),**(int**)(0x20+$esp)
end

define p_error
printf "p_error=%x, *p_error=%s\n",(char**)(0x18+$esp),*(char**)(0x18+$esp)
end

# will require $start and $size set.
define array
set $start =0xb7fdf000  
printf "%d %d %d %d %d %d %d %d\n", *(int *)($start + 16*0),*(int *)($start + 16*1),*(int *)($start + 16*2),*(int *)($start + 16*3),*(int *)($start + 16*4),*(int *)($start + 16*5),*(int *)($start + 16*6),*(int *)($start + 16*7)
end


