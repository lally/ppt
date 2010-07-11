#ifndef INCLUDE_MET_H
#define INCLUDE_MET_H

/*
 * CONFIGURATION PARAMETERS
 */
#define MET_VALUE_TYPE    double
#define MET_PRINTF_FORMAT "%8.4d"

/*
 * The API.
 */
#define MET_DEFENTRY_SINGLE(name)  MET_VALUE_TYPE val_##name
#define MET_DEFENTRY_AVERAGE(name) MET_VALUE_TYPE val_##name;	\
	                                unsigned char count_##name
#define MET_DEFENTRY_TOTAL(name)   MET_VALUE_TYPE val_##name

#define MET_SET_SINGLE(glob, name, value) \
	glob.val_##name = (MET_VALUE_TYPE) value
#define MET_SET_AVERAGE(glob, name, value) \
	glob.val_##name += (MET_VALUE_TYPE) value;	\
	glob.count_##name++
#define MET_SET_TOTAL(glob, name, value) \
	glob.val_##name += value

#define MET_CLEAR_SINGLE(glob, name) \
	glob.val_##name = (MET_VALUE_TYPE) 0.0;

#define MET_CLEAR_AVERAGE(glob, name) \
	glob.val_##name = (MET_VALUE_TYPE) 0.0;		\
	glob.count_##name = 0

#define MET_CLEAR_TOTAL(glob, name) \
	glob.val_##name = (MET_VALUE_TYPE) 0.0


 
#endif /* INCLUDE_MET_H */
