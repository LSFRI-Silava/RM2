
// Declaration of libraries to be used for compilation. They can be customized; in this case, a .h folder and a .cpp folder are included.
#include <strings.h>
#include "BluetoothSerial.h"
#include "SparkFun_u-blox_GNSS_Arduino_Library.h"
#include "WiFiHelper.h"
#include <HTTPClient.h>

//These are fixed variables, simplifications. For example, instead of writing BluetoothSerial each time, we write SerialBT; This makes the sketch more readable.
SFE_UBLOX_GNSS myGNSS;
BluetoothSerial SerialBT;
HTTPClient httpClient;

//These are the declared constants. These are values ​​that should not be modified.
const char* ssid = "LMT-2GHz-";
const char* password = "123456798";
const char* apiServer = "http://164.90.166.31/SparkFun-RTK-Surveyor/";
uint8_t address[6]  = {0xC4, 0xDD, 0x57, 0x67, 0x3D, 0x22}; //BT mac address

//These are mathematical operators. They are placed there to avoid overloading the rest of the code.
bool connected;
bool prev_gps_updated = false;

//The `void setup` statement is the beginning of code execution; the instruction chain will only be repeated once. It's very practical for starting functionalities, but for functions prone to failure, like Bluetooth, it's not recommended to include them solely there. Only a hard reset will restart this part.
void setup() {
  //Start the serial port for debug
  Serial.begin(9600);
  //initialisation sequence, 2 minutes to let time to the Wifi and bluetooth to start.
  Serial.println("2 minutes avant init");
  delay(60000);
  Serial.println("1 minute avant init");
  delay(30000);
  Serial.println("30 secondes avant init");
  delay(30000);
  Serial.println("init done");
  connectWifi(ssid,password);
  // As stated previously here the function connectBT is not implemented in the setup. it is just a call of a function defined below. 
  connectBT();
  connectGNSS();
}

//The void loop contains the instructions that will be repeated one after the other infinitely unless a blocking condition is included.
void loop() {
  //Check if the Serial Bluetooth is connected
  if(SerialBT.available()){
  //Format the string containing data to a JSON format. The code is written below  
  String jsondata = formatGPSDataToJSON();
  //Print the full string over serial port for debug
  Serial.println(jsondata);
  //Send the full string to the server
  sendDataToServer(jsondata);
  }
  //If the BT is not connected, make a re connection attempt. Hence why as previously mentionned it is important to not add this kind of function only in setup. 
  else{
    Serial.println("reconnect BT, patiente");
    connectBT();
  }
  //small delay to let enough time to the processor to process all data. 
  delay(200);
}

//Sketch specific to the bluetooth connection. at line 56 its written "connectBT()" when the processor read that, it execute the lines below from 64 to 83. It is very useful if you notice a BT disconnect at some point of the sketch, then you simply need to add connectBT();
void connectBT() {
   SerialBT.begin("ESP32", true); //Bluetooth device name
   SerialBT.setPin("0000");
   Serial.println("The device started, now you can pair it with bluetooth!");
  
  connected = SerialBT.connect(address);

  if(connected) {
      Serial.println(F("Connected Succesfully!"));
    } else {
      while(!SerialBT.connected(5000)) {
        Serial.println("Failed to connect. Make sure remote device is available and in range, then restart app.");
        connectBT(); 
      }
    }
    // disconnect() may take upto 10 secs max
    if (SerialBT.disconnect()) {
      Serial.println(F("Disconnected Succesfully!"));
    }
    // this would reconnect to the name(will use address, if resolved) or address used with connect(name/address).
    SerialBT.connect();
}

//Here it is the same story but to connect to the GNSS module via I2C
void connectGNSS() {
  if (myGNSS.begin(SerialBT) == false) //Connect to the Ublox module using BluetoothSerial
  {
    Serial.println(F("u-blox GPS not detected. Please check wiring. Freezing."));
    while (1);
  }
  Serial.println(F("u-blox module connected"));
}


// Here is the part containing instructions to send data to server. Very simple, connection, send, read answer. 
void sendDataToServer(String jsondata) {

    httpClient.begin(apiServer);
    httpClient.addHeader("Content-Type", "application/json");

    int httpCode = httpClient.POST(jsondata);

    //Check the returning code
    if (httpCode > 0) {
      String response = httpClient.getString();
      Serial.println("HTTP code: "+String(httpCode)+"; Response: "+response);
    } else {
      Serial.println(httpCode);
    }
    //Close connection
    httpClient.end();    
}



//Here is the funny part how to build the data string. It is needed to identify in each sensor answer which bit as to be collected. In the case of this GNSS module we are lucky because the parser is integrated. 
String formatGPSDataToJSON() {
  String time = getDateTime();
  String device = getMacAddress();

  uint32_t iTOW = myGNSS.getTimeOfWeekFromHPPOSLLH();
  uint16_t sat = myGNSS.getSIV();
  uint8_t fix = myGNSS.getCarrierSolutionType();
  int32_t lat = myGNSS.getHighResLatitude();
  int32_t lon = myGNSS.getHighResLongitude();
  int32_t alt = myGNSS.getAltitude();
  int32_t msl = myGNSS.getAltitudeMSL();
  uint32_t hacc = myGNSS.getHorizontalAccuracy();
  uint32_t vacc = myGNSS.getVerticalAccuracy();
  uint16_t hdop = myGNSS.getHorizontalDOP();
  
  String resp = "{";
  
  resp += "\"time\": \""+time+"\"";
  resp += ",\"device\": \""+device+"\"";
  resp += ",\"itow\": "+String(iTOW);
  resp += ",\"fix\": "+String(fix);
  resp += ",\"sat\": "+String(sat);
  resp += ",\"lat\": "+String(lat*0.0000001,8);
  resp += ",\"lon\": "+String(lon*0.0000001,8);
  resp += ",\"alt\": "+String(alt*0.001,2);
  resp += ",\"msl\": "+String(msl*0.001,2);
  resp += ",\"hacc\": "+String(hacc*0.0001,2);
  resp += ",\"vacc\": "+String(vacc*0.0001,2);
  resp += ",\"hdop\": "+String(hdop*0.01,2);
  resp += "}";

  return resp;
  Serial.print(resp);
}

String getMacAddress() {
  char macAddress[18];
  sprintf(macAddress, "%02X:%02X:%02X:%02X:%02X:%02X", address[0], address[1], address[2], address[3], address[4], address[5]);
  return String(macAddress);
}

String getDateTime() {
  char DateAndTimeString[20];
  sprintf(DateAndTimeString, "%4d-%02d-%02d %02d:%02d:%02d", myGNSS.getYear(), myGNSS.getMonth(), myGNSS.getDay(), myGNSS.getHour(), myGNSS.getMinute(), myGNSS.getSecond());
  return String(DateAndTimeString);
}
