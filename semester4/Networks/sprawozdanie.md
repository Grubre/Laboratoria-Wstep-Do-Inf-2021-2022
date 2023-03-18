# Sprawozdanie 1
## Ping
### Wrocławskie serwery
#### Nasza katedra (do 4 / od 3)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 4 -n 1 cs.pwr.edu.pl

Pinging cs.pwr.edu.pl [156.17.7.22] with 32 bytes of data:
Reply from 156.17.7.22: bytes=32 time=3ms TTL=61

Ping statistics for 156.17.7.22:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 3ms, Maximum = 3ms, Average = 3ms
```
#### Uniwersytet (do 11 / od 11)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 11 -n 1 uwr.edu.pl

Pinging uwr.edu.pl [156.17.87.85] with 32 bytes of data:
Reply from 156.17.87.85: bytes=32 time=4ms TTL=53

Ping statistics for 156.17.87.85:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 4ms, Maximum = 4ms, Average = 4ms
```
#### Spartan Kebab (do 8 / od 17)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 8 -n 1 spartankebab.eatbu.com

Pinging spartankebab.eatbu.com [34.89.135.24] with 32 bytes of data:
Reply from 34.89.135.24: bytes=32 time=25ms TTL=111

Ping statistics for 34.89.135.24:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 25ms, Maximum = 25ms, Average = 25ms
```
#### Wok in (do 15 / od 13)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 15 -n 1 wokin.pl

Pinging wokin.pl [213.32.10.111] with 32 bytes of data:
Reply from 213.32.10.111: bytes=32 time=37ms TTL=51

Ping statistics for 213.32.10.111:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 37ms, Maximum = 37ms, Average = 37ms
```
### Niedalekie duże miasta
#### Politechnika Poznańska (do 8 / od 9)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 8 -n 1 put.poznan.pl

Pinging put.poznan.pl [150.254.5.114] with 32 bytes of data:
Reply from 150.254.5.114: bytes=32 time=7ms TTL=55

Ping statistics for 150.254.5.114:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 7ms, Maximum = 7ms, Average = 7ms
```
#### UAM (do 9 / od 9)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 9 -n 1 amu.edu.pl

Pinging amu.edu.pl [150.254.65.170] with 32 bytes of data:
Reply from 150.254.65.170: bytes=32 time=8ms TTL=55

Ping statistics for 150.254.65.170:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 8ms, Maximum = 8ms, Average = 8ms
```
#### Galeria handlowa Avenida (do 15 / od 15)
```
PS C:\Users\serwi\Studia-WPPT> ping -i 15 -n 1 avenidapoznan.com

Pinging avenidapoznan.com [188.172.241.245] with 32 bytes of data:
Reply from 188.172.241.245: bytes=32 time=44ms TTL=49

Ping statistics for 188.172.241.245:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 44ms, Maximum = 44ms, Average = 44ms
```
#### Akademia nauk stosowanych Konin (do 9 / do 14)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 9 -n 1 ans.konin.pl

Pinging ans.konin.pl [188.210.221.82] with 32 bytes of data:
Reply from 188.210.221.82: bytes=32 time=32ms TTL=50

Ping statistics for 188.210.221.82:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 32ms, Maximum = 32ms, Average = 32ms
```
### Odległe Polskie miasta
#### Politechnika Białostocka (do 8 / od 7)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 8 -n 1 pb.edu.pl

Pinging pb.edu.pl [212.33.68.5] with 32 bytes of data:
Reply from 212.33.68.5: bytes=32 time=19ms TTL=57

Ping statistics for 212.33.68.5:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 19ms, Maximum = 19ms, Average = 19ms
```
#### Gmina Boćki (do 9 / od 14).
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 9 -n 1 gminabocki.pl

Pinging gminabocki.pl [89.161.158.221] with 32 bytes of data:
Reply from 89.161.158.221: bytes=32 time=27ms TTL=50

Ping statistics for 89.161.158.221:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 27ms, Maximum = 27ms, Average = 27ms
```
#### Miasto Hrubieszow (do 16 / od 14)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 16 -n 1 miasto.hrubieszow.pl

Pinging miasto.hrubieszow.pl [54.36.175.33] with 32 bytes of data:
Reply from 54.36.175.33: bytes=32 time=10ms TTL=50

Ping statistics for 54.36.175.33:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 10ms, Maximum = 10ms, Average = 10ms
```
### Serwery z państw sąsiednich
#### Freie Universität Berlin (do 16 / od 16)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 16 -n 1 fu-berlin.de

Pinging fu-berlin.de [160.45.170.10] with 32 bytes of data:
Reply from 160.45.170.10: bytes=32 time=36ms TTL=48

Ping statistics for 160.45.170.10:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 36ms, Maximum = 36ms, Average = 36ms
```
#### Charles University Praga (do 15 / od 13)
```
PS C:\Users\serwi\Studia-WPPT> ping -i 15 -n 1 cuni.cz

Pinging cuni.cz [195.113.89.35] with 32 bytes of data:
Reply from 195.113.89.35: bytes=32 time=31ms TTL=51

Ping statistics for 195.113.89.35:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 31ms, Maximum = 31ms, Average = 31ms
```
#### Vilnius University (Wilno) (do 13 / od 12)
```
PS C:\Users\serwi\Studia-WPPT> ping -i 13 -n 1 vu.lt

Pinging vu.lt [158.129.163.49] with 32 bytes of data:
Reply from 158.129.163.49: bytes=32 time=41ms TTL=243

Ping statistics for 158.129.163.49:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 41ms, Maximum = 41ms, Average = 41ms
```
#### Uniwersytet Lwowski (do 14 / od 15)
```
PS C:\Users\serwi\Studia-WPPT> ping -i 14 -n 1 lnu.edu.ua

Pinging lnu.edu.ua [116.203.247.192] with 32 bytes of data:
Reply from 116.203.247.192: bytes=32 time=34ms TTL=49

Ping statistics for 116.203.247.192:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 34ms, Maximum = 34ms, Average = 34ms
```
### Serwery z dalszych państw europejskich
#### Universite de Montpellier (do 16 / od 14)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 16 -n 1 umontpellier.fr

Pinging umontpellier.fr [193.51.152.74] with 32 bytes of data:
Reply from 193.51.152.74: bytes=32 time=76ms TTL=50

Ping statistics for 193.51.152.74:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 76ms, Maximum = 76ms, Average = 76ms
```
#### Cern Zurych (do 22 / od 16)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 22 -n 1 home.web.cern.ch

Pinging drupal-apps-shard-1.cern.ch [188.185.88.30] with 32 bytes of data:
Reply from 188.185.88.30: bytes=32 time=48ms TTL=48

Ping statistics for 188.185.88.30:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 48ms, Maximum = 48ms, Average = 48ms
```
#### Lotnisko Palma de Mallorca (do 19 / od 17)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 19 -n 1 aena.es

Pinging aena.es [194.224.177.158] with 32 bytes of data:
Reply from 194.224.177.158: bytes=32 time=58ms TTL=239

Ping statistics for 194.224.177.158:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 58ms, Maximum = 58ms, Average = 58ms
```
#### Strona rządowa Gibraltaru (do 14 / od 14)
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 14 -n 1 visitgibraltar.gi

Pinging visitgibraltar.gi [31.170.127.140] with 32 bytes of data:
Reply from 31.170.127.140: bytes=32 time=43ms TTL=52

Ping statistics for 31.170.127.140:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 43ms, Maximum = 43ms, Average = 43ms
```
### Serwery z Azji
### Serwery ze wschodniego wybreża Ameryki Północnej
### Serwery z zachodniego wybrzeża Ameryki Północnej
### Nowa Zelandia (Obliczanie średnicy internetu)
#### Urząd miasta Wellington (36 do / 35 od)
Najdalsza droga jaką udało mi się znaleźć.
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 36 -n 1 wellington.govt.nz

Pinging wellington.govt.nz [52.64.224.120] with 32 bytes of data:
Reply from 52.64.224.120: bytes=32 time=308ms TTL=231

Ping statistics for 52.64.224.120:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 308ms, Maximum = 308ms, Average = 308ms
PS C:\Users\serwi\Studia-WPPT> ping -i 35 -n 1 wellington.govt.nz

Pinging wellington.govt.nz [52.64.224.120] with 32 bytes of data:
Request timed out.

Ping statistics for 52.64.224.120:
    Packets: Sent = 1, Received = 0, Lost = 1 (100% loss),
```
## Egzotyczne miejsca
#### Strona rządu Sri Lanki (do 21 / od 17)
```
ping -i 21 -n 1 gov.lk

Pinging gov.lk [43.224.124.136] with 32 bytes of data:
Reply from 43.224.124.136: bytes=32 time=177ms TTL=239

Ping statistics for 43.224.124.136:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 177ms, Maximum = 177ms, Average = 177ms
```
``
