;DHT Humidity/Temperature Sensor Library for Great Cow Basic

;Thomas Henry
;Version 2.0 -- 4/21/2014

;This include file will let you easily exploit the
;DHT11 or DHT22 relative humidity/temperature sensors
;in your own programs. The sensors share the same pinouts
;and obey a similar data transmission protocol. The main
;differences are in the range, accuracy and decimal
;representation. In particular, quoting from the spec
;sheets:

;DHT11: senses to the nearest whole number
;RH:        20% to 90%,   +/- 5%
;Temp (C):  0° to 50°,    +/- 2°
;Temp (F):  32° to 122°,  +/- 3.6°

;DHT22: senses to the nearest tenth
;RH:        0% to 100%,   +/- 2% (5% worst case)
;Temp (C):  -40° to 125°, +/- 0.1°
;Temp (F):  -40° to 176°, +/- 3.3°

;Two constants are required in the main calling program.
;The first sets the device type, i.e.:

;#DEFINE DHT_TYPE 11

;or

;#DEFINE DHT_TYPE 22

;The other constant determines the single data pin, e.g.:

;#DEFINE DHT_PIN PORTA.0

;or

;#DEFINE DHT_PIN PORTA.1

;Note that whichever pin is employed for these, it must
;be capable of both input and output operation and must
;have a 10k pull-up resistor.

;For reference, the identical pinouts of the DHT11 and
;DHT22 are, looking from the front, (grill side):

;Pin 1: +5V
;Pin 2: DHT_PIN (the data pin)
;Pin 3: no connection
;Pin 4: ground

;These sensors transmit data via pulse code modulation at a
;fairly zippy rate, so the microcontroller clock should be
;at least 8 MHz. Observe that the DHT11 should not be polled
;more frequently than once a second and the DHT22 more than
;once every two seconds.

;There is one subroutine:

;readDHT(DHT_rh, DHT_cels, DHT_fahr, DHT_error)

;The values returned are, respectively, the relative humidity,
;temperature in Celsius, temperature in Fahrenheit, and an
;error indicator.

;The humidity and temperatures are signed integers, the error
;indicator a byte. For the DHT11, the numerical results are
;simply whole numbers. For the DHT22 numerical results
;are fixed pointed numbers, with the decimal point assumed one
;digit from the right. In other words, the values are scaled up
;by 10. For example, a return value of 657 for the relative
;humidity would be interpreted as 65.7%.

;Also returned for either sensor is an error condition (a byte):

;0 = no error
;1 = no response from the sensor
;2 = bad checksum from the sensor

;Nine local bytes are consumed in the computations, and
;seven bytes are used for the output parameters. That's a grand
;total of sixteen bytes required when invoking these.

;Conditional compilation is employed. The value of DHT_TYPE
;determines what code is compiled.

;----- Variables

dim DHT_values(5)
dim DHT_counter, DHT_i, DHT_byte as byte

;reuse a couple variables to save memory
dim DHT_chksum alias DHT_counter
dim DHT_abs alias DHT_counter

sub readDHT(out DHT_rh as integer, out DHT_cels as integer, out DHT_fahr as integer, out DHT_error as byte)

  ;----- request an update

  dir DHT_pin out                       ;port is output now
  set DHT_pin off                       ;go low
  wait 18 mS                            ;for 18 milliseconds
  set DHT_pin on                        ;then go high
  wait 40 uS                            ;for 40 microseconds

  ;----- wait for an acknowledgment

  dir DHT_pin in                        ;port is input now

  DHT_counter = 0                       ;count by tens of microseconds
  do while DHT_pin = off                ;until pin goes high
    wait 1 10uS
    DHT_counter++
    if DHT_counter > 9 then             ;should take 80 microseconds
      goto DHT_noResponse               ;so must be a dud
    end if
  loop

  DHT_counter = 0                       ;repeat, looking for a low now
  do while DHT_pin = on
    wait 1 10uS
    DHT_counter++
    if DHT_counter > 9 then             ;should take 80 microseconds
      goto DHT_noResponse               ;so must be a dud
    end if
  loop

  ;----- start of transmission

  for DHT_i = 1 to 5                    ;five bytes to collect
    DHT_byte = 0                        ;build up byte here
    repeat 8                            ;8 bits to a byte
      DHT_byte = 2 * DHT_byte           ;shift left one place
      do while DHT_pin = off            ;wait for start of pulse
      loop

      DHT_counter = 0                   ;measure incoming pulse
      do while DHT_pin = on
        wait 1 10uS                     ;count by tens of microseconds
        DHT_counter++
      loop

      if DHT_counter > 4 then           ;long pulse is a 1
        DHT_byte++                      ;factor the bit in
      end if
    end repeat                          ;go do next bit

    DHT_values(DHT_i) = DHT_byte        ;store complete byte
  next DHT_i

  dir DHT_pin out                       ;go back to quiescence
  set DHT_pin on

  ;----- construct output values

  DHT_chksum = DHT_values(1)            ;compute the checksum
  for DHT_i = 2 to 4
    DHT_chksum = DHT_chksum + DHT_values(DHT_i)
  next DHT_i

  if DHT_chksum <> DHT_values(5) then
    DHT_error = 2                       ;Error 2 bad checksum
    goto DHT_conclude
  else
    DHT_error = 0                       ;Error 0 okay

    ;next bit of code is for the DHT11 only
    #if DHT_type = 11
      DHT_rh = DHT_values(1)   ;get humidity
      DHT_cels = DHT_values(3) ;get Celsius

      ;compute Fahrenheit rounded to the nearest whole number
      DHT_fahr = [integer]10*DHT_cels
      DHT_fahr = [integer](DHT_fahr*9/5+325)/10
    #endif

    ;the next bit of code is for the DHT22 only
    #if DHT_type = 22
      ;compute relative humidity scaled up by 10
      DHT_rh = [integer]256 * DHT_values(1) + DHT_values(2)

      ;save the absolute value of the Celsius temperature
      DHT_abs = DHT_values(3) & 0x7F

      ;compute Celsius temperature scaled up by 10
      DHT_cels = [integer]256 * DHT_abs + DHT_values(4)

      ;factor in the sign if needed
      if DHT_values(3).7 = on then      ;a negative temp
        DHT_cels = -DHT_cels            ;make a true 2's complement
      end if

      ;compute Fahrenheit rounded to the nearest tenth
      DHT_fahr = [integer]10*DHT_cels
      DHT_fahr = [integer](DHT_fahr*9/5+3205)/10
    #endif

    goto DHT_conclude                   ;all done!
  end if

  DHT_noResponse:
    DHT_error = 1                       ;Error 1 no response
  DHT_conclude:
end sub