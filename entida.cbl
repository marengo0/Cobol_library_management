       IDENTIFICATION DIVISION.  
       program-id. sient01m.                                                    
      *                                                                         
      *------------- mantenimiento de entidades
      *                                                                         
       environment division.                                                    
       configuration section.                                                   
       source-computer.  rmcobol-85.                                            
       object-computer.  rmcobol-85.                                            
       special-names.                                                           
           decimal-point is comma.                                              
           CRT STATUS IS tecla.
                                                                         
       input-output section.                                                    
       file-control.                                                            
      *                                                                         
                                                                                
      *     copy 'sient01.sel'.                                                  
           select ent assign to random, 'sient01.fic'
                  organization indexed
                  access mode dynamic
                  record key is ent-clave
                  file status is fs-ent.


                                                                                
      *                                                                         
       data division.                                                           
       file section.                                                            
      *                                                                         
                                                                                
      *     copy 'sient01.fd'.                                                   
úúúúúú*úúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúú
      *  FICHEROS DEL SISTEMA.
úúúúúú*úúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúú
       fd  ent.
       01  reg-ent.
           03 ent-clave         pic x(11).
           03 ent-nombre        pic x(40).
           03 ent-pass          pic x(15).
           03 ent-comentario    pic x(40).
           03 filler pic x(406).

                                                                                
 *******                                                                        
       working-storage section.                                                 
*******                                                                         
       01  status-ficheros.                                                     
           02 fs-ent            pic xx.                                         
              88 esta-ent             value '00' '02'.                          
              88 n-esta-ent           value '23'.                               
              88 fin-ent              value '46'  '10'.                         
              88 bloqueado-ent        value '99'.                               
              88 f-bloqueado-ent      value '38' '93'.                          
              88 f-noexiste-ent       value '35'.                               
                                                                                
       01 fx              pic x(4).                                             
       01 f-ficheros.                                                           
          03 f-nombre     pic x(7).                                             
          03 f-error      pic xx.                                               
          03 f-que        pic x(6) value 'leer  '.                              
          03 f-mensaje    pic x(70).                                            
          03 f-coment     pic x(30).                                            
                                                                                
       01 nombre-pantalla pic x(80) value 'sient01m.cr0'.                       
       01 nombre-ent      pic x(80) value 'sient01.fic'.                        
                                                                                
       01 alta         pic x.                                                   
                                                                                
       01  pantalla.                                                            
           03 p-clave       pic x(11).                                           
           03 p-nombre      pic x(40).                                          
           03 p-pass        pic x(15).                                          
           03 p-pass2       pic x(15).                                          
           03 p-comentario  pic x(40).                                          
                                                                                
       01  aux            pic 99    value 0 .                                   
       01  x              pic 99.                                               
       01  y              pic 99.                                               
       01  mayus          pic x(5) value 'UPPER'.                               
       01  sw99           pic 9 value 0.                                        
       01  p-ent-formato  pic x value ' '.                                      
                                                                                
       01  mun00-entblo   pic 9(5).                                             
------*                                                                         
      *     copy 'siworfs.cpy'.                                                  
      *     copy 'siwortecla.cpy'.                                                 
------*     teclaiables de control
       01  ristra      pic x(8) value ' '.
       01  fecha       pic 9(6) value 0. 
      *                                                ----OpenCobol--- 
       01  tecla              pic 9999 value 0. 
           88 f1               value   01                    1001. 
           88 f2               value   02                    1002. 
           88 f3               value   03                     1003. 
           88 f4               value   04                    1004.
           88 f5               value   05, 06           1006   1005.
           88 f6               value   06, 05            1006   1005.
           88 f7               value   07                    1007. 
           88 f8               value   08. 
           88 f9               value   09. 
           88 f10              value   10. 
           88 f11              value   11, 27.
           88 f12              value   12.
           88 f13              value   13.
           88 f14              value   14.
           88 f15              value   15, 01.
           88 f16              value   16.
           88 ayuda            value   15, 01.
           88 ejecutar         value   16.
           88 borr             value   05, 06               1005 1006.
           88 escapar          value   27, 11, 09.
      * Se pone el valor 09 en la teclaiable escape para que en Xenix funcione
      * tal y como hicimos en la N¢mina. (especial para Le¢n y Almer¡a).
           88 esc              value   27, 11.
           88 cursor-up        value   52. 
           88 cursor-dw        value   53. 
           88 cursor-iz        value   07, 56. 
           88 pg-up            value   67. 
           88 pg-dn            value   68. 
           88 intro            value   13                       0. 
           88 tabulador        value   58, 09.
       01  texto         pic x(80) value spaces.
       01  texto1        pic x(80) value spaces. 
       01  continua      pic x(19) value '<intro> continua...'. 
       01  goma          pic x(80) value spaces. 
       01  raya          pic x(30) value all '_'. 
       01  nada            pic x value ' '. 
       01  w-fecha.
           02 w-eje        pic 99.
           02 w-mes        pic 99.
           02 w-dia        pic 99.
       01  calenda.
              06 ca-ano   pic 99 value 0. 
              06 ca-mes   pic 99 value 0. 
              06 ca-dia   pic 99 value 0. 
       01  resto          pic 99.
       01  resul          pic 99 value zeros.
       01  anno           pic 9(4) value zeros.
       01  calenda1       pic 9(6) value 0. 
       01  longitud       pic 99 value 0.
       01  conforme        pic x value ' '. 
           88 siconforme         value 'S', 's'. 
           88 noconforme         value 'N', 'n'. 
       01  i            pic 9(5) value 0. 
       01  j            pic 9(5) value 0. 
       01  k            pic 9(5) value 0. 
       01  que             pic x value ' '. 
       01  color0       pic x(37) value "fcolor=red, bcolor=white".
       01  color1      pic x(37) value "fcolor= WHITE, bcolor=BLue".
       01  color2      pic x(37) value "fcolor= WHITE, bcolor=cyan".
       01  color3      pic x(37) value "fcolor= WHITE, bcolor=BLue".
       01  lk-datos.
           03 lk-numero      pic 9(04).
           03 lk-nombre-file pic x(40).
           03 lk-error       pic x(04).
------* 
       01  tabla1.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 29.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 30.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 30.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 30.
           03 filler pic 9(2) value 31.
           03 filler pic 9(2) value 30.
           03 filler pic 9(2) value 31.
       01  rtabla1 redefines tabla1.
           03 dias pic 99 occurs 12 times.
       01  tabla2.
           03  filler pic x(31) value 'Revise la Fecha, Mes incorrecto'.
           03  filler pic x(31) value 'Revise la Fecha, Dia incorrecto'.
           03  filler pic x(31) value 'Revise la Fecha, a#o incorrecto'.
       01  rtabla2 redefines tabla2.
           03 teto pic x(31) occurs 3 TIMES.
      *
       01  tabla-meses.
           02 filler pic x(10) value 'ENERO     '.
           02 filler pic x(10) value 'FEBRERO   '.
           02 filler pic x(10) value 'MARZO     '.
           02 filler pic x(10) value 'ABRIL     '.
           02 filler pic x(10) value 'MAYO      '.
           02 filler pic x(10) value 'JUNIO     '.
           02 filler pic x(10) value 'JULIO     '.
           02 filler pic x(10) value 'AGOSTO    '.
           02 filler pic x(10) value 'SEPTIEMBRE'.
           02 filler pic x(10) value 'OCTUBRE   '.
           02 filler pic x(10) value 'NOVIEMBRE '.
           02 filler pic x(10) value 'DICIEMBRE '.
       01  tab-meses redefines tabla-meses.
           03 ele-mes occurs 12.
              05 t-mes    pic x(10).
       
        01 nombre-ayunt     pic x(40).
        01  tecla-centrar.
            03 ajax             pic s9(2).
            03 centro           pic 99.
            03 blanco           pic x(70) value ' '.
        01 listado-v-h pic x value 'V'.
            88 es-listado-h value 'H' 'h'.
            88 es-listado-v value 'V' 'v'.
        01 version-ex  is external pic x.
            88 version-windows  value 'w' 'W'.
        01 opcion-menu-ex pic x(60) is external.
        01 opcion-visor pic x is external.
            88 es-visor-1 value '1'.
            88 es-visor-2 value '2'.
            88 es-visor-3 value '3'.
            88 es-visor-4 value '4'.
            88 es-visor-5 value '5'.
            88 es-visor-6 value '6'.
            88 es-visor-7 value '7'.
            88 es-visor-8 value '8'.
            88 es-visor-9 value '9'.
            88 es-visor-a value 'a' 'A'.
            88 es-visor-r value 'r' 'R'.
            88 es-visor-s value 's' 'S' 'X' 'x'.
            88 es-visor-x value 'X' 'x'.
               
       01 lk-using.                                                             
          03 lk-caminos  pic 9(3) occurs 20.                                    
          03 lk-ipre.                                                           
             05 lk-fecha  pic 9(6) value 0.                                             
             05 lk-col    pic 999.                                              
             05 lk-lon    pic 99.                                               
             05 lk-lm     pic x.                                                
                88 lk-laser value 'l'.                                          
             05 lk-ent    pic 9(5).                                             
             05 lk-inicializa  pic x(70).                                       
             05 lk-restaura    pic x(70).                                       
      *   en lk-caminos(4)  esta el camino de la impresora por defecto.         
          03 lk-texto    pic x(68).                                             
               SCREEN SECTION.
       01 CLEAR-SCREEN.
           05 BLANK SCREEN BACKGROUND-COLOR 0.                                                                        
                                                                                
------*                                                                         
       procedure division.                                                      
       declaratives.                                                            
       errores section.                                                         
           use after standard error procedure on  ent .          
       end declaratives.                                                        
       programa section.                                                        
       programa-prin. 
           perform sacar-pantalla.                                                          
       inicio.                                                            
           perform open-io-ent                                                  
           if f-noexiste-ent                                                    
               display   ' Fichero ent01 no existe, pulse "S" si desea c        
      -               'rearlo.'                                                 
                      line 24 position 1                                        
               accept nada line 24 position 57                                  
               if not (nada = 's' or 'S')                                       
                  go acabamos                                                   
               else                                                             
                  perform open-o-ent                                            
                  perform close-ent                                             
                  perform open-io-ent                                           
               end-if                                                           
               display goma line 24 position 1                                  
           else                                                                 
               if fs-ent   not = '00'                                           
                  move 'ent01' to f-nombre                                      
                  move fs-ent to f-error                                        
                  move 'abrir' to f-que                                       
                  perform sacar-error                                           
                  go acabamos                                                   
               end-if                                                           
           end-if.                                                              
           move ' ' to ent-clave                                                  
           perform read-ent                                                     
                                                                                
           perform pedir-clave thru fin-pedir-clave                             
                                                                                
           perform until escapar                                                
              display goma line 24 position 1                                   
              perform read-ent                                                  
              if n-esta-ent                                                     
      *        --------------------------- ALTA                                 
                 initialize reg-ent 
                 move p-clave to ent-clave
                 move 'n' to conforme                                           
                 initialize pantalla                                            
                 move ent-clave to p-clave                                      
                 display ' ** Alta **        '  line 4 position 55              
                 move 's' to alta                                               
                 move ' ' to p-comentario                              
                                                                                
                 display p-nombre  line 7 position 27  reverse               
                 display p-comentario line 9 position 27 reverse                  
                 display p-pass    line 11 position 27 reverse                  
                                                                                
                 perform pedir-datos thru fin-pedir-datos                       
                                                                                
                 if siconforme                                                  
                    perform write-ent                                           
                    if not esta-ent                                             
                       move 'ent01' to f-nombre                                 
                       move fs-ent to f-error                                   
                       move 'grabar' to f-que                                   
                       perform sacar-error                                      
                    else                                                        
                       display '*Grabado*   ' line 4 position 55                
                    end-if                                                      
                 end-if                                                         
              else                                                              
                 if esta-ent                                                    
      *         ----------------------------- MODIFICACION                      
                    display ' ** Modificacion **'                               
                          line 4 position 55                                    
                    move 'n' to alta conforme                                   
                    move ent-nombre to p-nombre                                 
                    move ' ' to p-pass p-pass2                                  
                    move ent-comentario  to p-comentario                              
                    display p-nombre  line 7 position 27 reverse                
                    display p-comentario line 9 position 27 reverse               
                    display p-pass    line 11 position 31 reverse               
                    if sw99 = 0                                                 
                       perform pedir-datos thru fin-pedir-datos                 
                    else                                                        
                       move ' ' to conforme                                     
                    end-if                                                      
                    if f5 or f6                                                 
                       perform delete-ent                                       
                       initialize reg-ent                                       
                       move 'n' to conforme                                     
                       perform programa-prin                                    
                    end-if                                                      
                    if siconforme  and not escapar                              
                       perform rewrite-ent                                      
                       if not esta-ent                                          
                          move 'ent01' to f-nombre                              
                          move fs-ent   to f-error                              
                          move 'grabar' to f-que                                
                          perform sacar-error                                   
                       else                                                     
                          display '*Modificado*       '                         
                              line 4 position 55                                
                       end-if                                                   
                    end-if                                                      
                 else                                                           
                    move 'ent01' to f-nombre                                    
                    move fs-ent to f-error                                      
                    move 'leer  ' to f-que                                      
                    perform sacar-error                                         
                 end-if                                                         
                                                                                
              end-if                                                            
              perform pedir-clave thru fin-pedir-clave                          
           end-perform.                                                         
------*                                                                         
       se-acabo.                                                                
           perform close-ent.                                                    
       acabamos.                                                                
           goback.                                                              
------*                                                                         
                                                                                
                                                                                
       pedir-clave.                                                             
           perform test after until (intro and p-clave > 0) or f2               
                                    or f7 or escapar 
              display 
               '<F1> Ayuda <F2> Siguiente <F3> Usuarios <F7> Clave <F4> 
      -        'Accesos web'              
               line 24 position 1 erase eol                                     
              accept p-clave line 5 position 27 update tab prompt                                                       
              end-accept     
              display tecla line 1 position 60 reverse
              if f15 then                                                    
                   call 'sient01t'                                          
                       using lk-using, p-clave on exception 
                       continue end-call
                   cancel 'sient01t'
                   perform sacar-pantalla
              end-if                                                         
              if f3 then                                                    
                   call 'sient02m'                                          
                       using lk-using, p-clave on exception 
                       continue end-call
                   cancel 'sient02m'
                   perform sacar-pantalla
              end-if                                                         
              if f4 then                                                    
                   call 'siweb00m'                                          
                       using lk-using, p-clave on exception 
                       continue end-call
                   cancel 'siweb00m'
                   perform sacar-pantalla
              end-if                                                         
           end-perform.                                                         
           move 0 to sw99                                                       
           if f2                                                                
              move p-clave to ent-clave
              perform start-ent                                                 
              perform read-next-ent                                             
              if p-clave = ent-clave                                            
                 perform read-next-ent                                          
              end-if                                                            
              move ent-clave to p-clave                                         
              move 1 to sw99                                                    
              if p-clave = 0 perform read-next-ent                              
                 move ent-clave to p-clave                                      
              end-if                                                            
           end-if.                                                              
           if f7                                                                
             display ent-pass line 11 position 27 reverse                       
             go pedir-clave
           end-if.                                                              
                                                                                
           display p-clave line 5 position 27 reverse                       
           move p-clave to ent-clave.                                           
                                                                                
       fin-pedir-clave.                                                         
           exit.                                                                
                                                                                
       pedir-datos.                                                             
           move 'n' to conforme.                                                
           if alta = 'n'                                                        
              display '<F5>/<F6>' line 24 position 1  erase eol             
              display 'Borrar Entidad.' line 24 position 11                  
           end-if.                                                              
                                                                                
           perform test after until intro or escapar or cursor-up               
               or f5 or f6                                                      
              accept p-nombre line 7 position 27 update tab prompt              
                                
              end-accept                                                        
           end-perform.                                                         
           display p-nombre line 7 position 27 reverse                      
           move p-nombre to ent-nombre                                          
           if cursor-up move 27 to tecla end-if                                  
           if escapar or ( f5 or f6 and alta = 'n' )                            
              go fin-pedir-datos.                                               
                                                                                
       pedir-comentario.                                                           
           perform test after until intro or escapar or cursor-up               
              accept p-comentario   line 9 position 27                            
                    update tab prompt                          
              end-accept                                                        
           end-perform.                                                         
           if cursor-up go pedir-datos.                                         
           move p-comentario to ent-comentario.                                       
                                                                                
       pide-conforme.                                                           
           move ' ' to conforme                                                 
           display '<F2> ' line 24 position 1 erase eol                    
           display 'Cambiar palabra clave.' line 24 position 6.             
           perform test after until cursor-up or ((siconforme or                
                  noconforme) and intro) or f2                                  
                accept conforme line 21 position 62 update tab prompt           
                          reverse  
                end-accept                                                      
           end-perform.                                                         
           display ' ' line 21 position 62 reverse                          
           if f2                                                                
              display ' ' line 24 position 1 erase eol                          
              perform pedir-pass                                                
              go pide-conforme                                                  
           end-if                                                               
           if cursor-up                                                         
              display ' ' line 24 position 1 erase eol                          
              go pedir-comentario                                                   
           end-if.                                                              
                                                                                
       fin-pedir-datos.                                                         
                                                                                
       pedir-pass.                                                              
           perform test after until intro or escapar or cursor-up               
              accept p-pass  line 11 position 27 off update tab prompt            
              end-accept                                                        
           end-perform.                                                         
           display 'Vuelva a introducir la palabra clave.' line 24              
                 position 1 erase eol                                           
           perform test after until intro or escapar or cursor-up               
              accept p-pass2 line 11 position 27 off update tab prompt           
           end-perform.                                                         
           if p-pass not = p-pass2                                              
              display 'Las palabras clave son diferentes, no se ha modif        
      -           'icado. Pulse tecla ...' line 24 position 1 erase eol         
              accept que line 24 position 79                                    
           else                                                                 
              move p-pass      to ent-pass                                      
           end-if.                                                              
                                                                                
       sacar-pantalla.                                                                         
      *  esto es un gestor de pantallas--> lo cambio por displays 
      *     call 'volcado1' using nombre-pantalla.  
           DISPLAY CLEAR-SCREEN
           display ' programa de prueba     ³ MANTENIMIENTO Entidades
      -     '                  ³' line 1 position 1
           display 'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -     'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ' line 2 position 1.
           display '      Codigo de Entidad.: ___________
      -     '' line 5 position 1.
           display '      Nombre de Entidad.: __________________________
      -     '______________' line 7 position 1.
           display '      Comentario........: __________________________
      -     '______________' line 9 position 1.
           display '      Palabra de Paso.. : _______________
      -     '' line 11 position 1.
           display '                                            CONFORME
      -     ' (S/N).: _' line 21 position 1.
           display 'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -     'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ '
                         line 22 position 1.
      *     cancel 'volcado1'                                                    
           perform sacar-fecha.                                                 
                                                                                
                                                                                
                                                                                
       sacar-fecha.                                                             
      *     copy 'siwdia.cpy'.                                                   
                                                                                
      *  copy 'sierrorf.cpy'.                                                    
      *-------------------------------------------------------------------
        sacar-error.
           call 'C$RERR' using fx
           cancel 'C$RERR'
           if f-que = ' '
              move 'leer  ' to f-que
           end-if
           initialize f-mensaje
           if f-error = '99'
             string 'Registro ocupado por otro proceso en fichero '
                f-nombre '.#' delimited by '#' f-coment delimited by
                size into f-mensaje
           else
             if f-error = '23'
                string 'Registro no encontrado en fichero ' f-nombre
                   ' al ' f-que '.#' delimited by '#' f-coment delimited
                   by size into f-mensaje
             else
               if (f-error = '38' or = '93' or = '90')
                 string 'Fichero bloqueado ' f-nombre
                   ' al ' f-que '.#' delimited by '#' f-coment delimited
                   by size into f-mensaje
               else
                 if f-error = '35'
                    string 'Fichero ' f-nombre
                      ' no existe.' f-coment delimited
                      by size into f-mensaje
                 else
                  if (f-error = '46' or = '10')
                    string 'Fin del fichero ' f-nombre
                      '.' delimited  by size into f-mensaje
                  else
                    string 'Error ' fx(1:2) ',' fx(3:2) ' en fichero '
                      f-nombre ' al ' f-que '.#' delimited by '#'
                      f-coment delimited by  size into f-mensaje
                  end-if
                 end-if
               end-if
             end-if
           end-if
           display f-mensaje line 24 position 1 erase eol
           accept que line 24 position 79
           end-accept
           initialize f-coment
           display ' ' line 24 position 1 erase eol.

      *----------------------------------------------------------------*
      * -------------- ESTRUCTURA DE DATOS NECESARIA ----------------- *
      *  01 fx             pic x(4).                                   *
      *  01 f-ficheros.                                                *
      *     03 f-nombre    pic x(7).                                   *
      *     03 f-error     pic xx.                                     *
      *     03 f-que       pic x(6)   value 'leer  '.                  *
      *     03 f-mensaje   pic x(79).                                  *
      *     03 f-coment    pic x(35).                                  *
      *----------------------------------------------------------------*

      *  copy 'sient01.pro'.                                                     
      *-------- todos los posibles accesos a los ficheros:

      *                                         XAPLGAS
      *
       open-i-ent.
           open input ent.
       open-o-ent.
           open output ent.
       open-io-ent.
           open i-o ent.
      *
       read-ent.
           read ent invalid key continue.
       read-ent-no-lock.
           read ent with no lock invalid key continue.
      *
       start-ent.
           start ent key not less than ent-clave
                 invalid key continue.
       read-next-ent.
           read ent next record with no lock at end continue.

       delete-ent.
           delete ent invalid key continue.
      *
       write-ent.
           write reg-ent invalid key continue.
       rewrite-ent.
           rewrite reg-ent invalid key continue.
       close-ent.
           close ent.



