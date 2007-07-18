/*****************************************************************************/
/*                                                                           */
/* builtins/io.h{c}                                                          */
/*****************************************************************************/

#ifndef IO_H
#define IO_H

void BIIO_openIn();
void BIIO_openOut();
void BIIO_openApp();
void BIIO_closeIn();
void BIIO_closeOut();
void BIIO_openStr();
void BIIO_input();
void BIIO_output();
void BIIO_inputLine();
void BIIO_lookahead();
void BIIO_eof();
void BIIO_flush();
void BIIO_print();
void BIIO_read();
void BIIO_printTerm();
void BIIO_termToStr();
void BIIO_strToTerm();
void BIIO_readTerm();
void BIIO_getEnv();
void BIIO_openSocket();
void BIIO_unixTime();


#endif  //IO_H
