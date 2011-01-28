/* platform-agent.h
   ----------------

   Declarations for the stuff that's implemented separately for each
   platform.
*/

#ifndef INCLUDE_PLATFORM_AGENT_H
#define INCLUDE_PLATFORM_AGENT_H

/* API NOTES 
   ---------

   We have to deal with the fact that different ELF symbols are mapped
   to different objects, which are then translated into the address
   space.

   So, (1) get the list of elf objects, and (2) translate their
   symbols' addresses into the mapped address space.

   The question is, how do we determine the elf object to use?  We
   have the executable and shared libraries.  Default to the
   executable unless we (a) fail or (b) get an argument.  For the fail
   case, what's the ordering of the search?

   For now, stick with the executable.  For libraries, we may jump to
   taking a command-line arg, but a fallback list is just asking for
   problems.  A command-line arg asking for that behavior, however,
   may be useful.  

 */

#include <string>

/* Given a pid, return a pathname to the executable */
const std::string plat_exec_pathname(pid_t pid);

/* Given a pid, a pathname returned from 'plat_exec_pathname', and an
   offset into that file (retrieved from gelf(3c)), return the
   location a symbol in the address space of 'pid'.  */
off_t plat_sym_loc(pid_t pid, const std::string& object, off_t start_addr);

/* Given a pid, return an open file handle to its addres space.
   Return value is the same as open(2).  */
int plat_pid_address_space(pid_t pid);

bool plat_write_address(int handle, pid_t pid, off_t sym_off, int sym_value);
int plat_read_address(int handle, pid_t pid, off_t sym_off);

void plat_pid_close_address_space(int handle);
#endif /* #ifndef INCLUDE_PLATFORM_AGENT_H */
