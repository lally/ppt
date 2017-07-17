emit C++;

buffer Minimal 512;

option runtime multithread true;
option time timespec realtime;
option debug true;

frame first {
   int a, b, c;
   interval time duration;
   interval counter events;
}

frame second {
   interval counter foos;
}


