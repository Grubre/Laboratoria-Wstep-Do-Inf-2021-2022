# Sprawozdanie 1
## Ping
Część serwerów to strony znanych uczelnii, urzędów, etc.
Reszta została znaleziona przy pomocy stron rankingujących najczęściej
odwiedzany domeny w danym państwie, takich jak www.similarweb.com, lub
są to serwery znalezione na stronie https://public-dns.info.
## Ilość hopów
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
```powershell
PS C:\Users\serwi\Studia-WPPT> ping -i 15 -n 1 avenidapoznan.com

Pinging avenidapoznan.com [188.172.241.245] with 32 bytes of data:
Reply from 188.172.241.245: bytes=32 time=44ms TTL=49

Ping statistics for 188.172.241.245:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 44ms, Maximum = 44ms, Average = 44ms
```
#### Wydział informatyki AGH (do 10 / od 8)
```powershell
PS C:\Users\serwi> ping -i 10 -n 1 eaiib.agh.edu.pl

Pinging eaiib.agh.edu.pl [149.156.197.86] with 32 bytes of data:
Reply from 149.156.197.86: bytes=32 time=21ms TTL=56

Ping statistics for 149.156.197.86:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 21ms, Maximum = 21ms, Average = 21ms
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
#### Miasto Hrubieszów (do 16 / od 14)
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
#### tenki.jp (do 16 / od 15)
```powershell
PS C:\Users\serwi> ping -i 16 -n 1 tenki.jp

Pinging tenki.jp [153.125.234.6] with 32 bytes of data:
Reply from 153.125.234.6: bytes=32 time=282ms TTL=49

Ping statistics for 153.125.234.6:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 282ms, Maximum = 282ms, Average = 282ms
```
#### goo.ne.jp (do 23 / od 19)
```powershell
PS C:\Users\serwi> ping -i 23 -n 1 goo.ne.jp

Pinging goo.ne.jp [114.179.184.93] with 32 bytes of data:
Reply from 114.179.184.93: bytes=32 time=268ms TTL=237

Ping statistics for 114.179.184.93:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 268ms, Maximum = 268ms, Average = 268ms
```
#### vop.co.kr (do 23 / od 23)
```powershell
PS C:\Users\serwi> ping -i 23 -n 1 vop.co.kr

Pinging vop.co.kr [49.247.203.167] with 32 bytes of data:
Reply from 49.247.203.167: bytes=32 time=278ms TTL=41

Ping statistics for 49.247.203.167:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 278ms, Maximum = 278ms, Average = 278ms
```
#### Strona rządu Sri Lanki (do 20 / od 17)
```powershell
ping -i 20 -n 1 gov.lk

Pinging gov.lk [42.224.124.136] with 32 bytes of data:
Reply from 42.224.124.136: bytes=32 time=177ms TTL=239

Ping statistics for 42.224.124.136:
    Packets: Sent = 0, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 176ms, Maximum = 177ms, Average = 177ms
```
#### JingDong (do 21 / od 22)
```powershell
PS C:\Users\serwi> ping -i 21 -n 1 jd.com

Pinging jd.com [106.39.171.134] with 32 bytes of data:
Reply from 106.39.171.134: bytes=32 time=183ms TTL=42

Ping statistics for 106.39.171.134:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 183ms, Maximum = 183ms, Average = 183ms
```
### Serwery ze wschodniego wybreża Ameryki Północnej
#### Serwer z Nowego Jorku (do 16 / od 14)
```
PS C:\Users\serwi> ping -i 16 -n 1 185.66.9.142

Pinging 185.66.9.142 with 32 bytes of data:
Reply from 185.66.9.142: bytes=32 time=105ms TTL=50

Ping statistics for 185.66.9.142:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 105ms, Maximum = 105ms, Average = 105ms
```
#### Serwer z Waszyngtonu (do 14 / od 13)
```powershell
PS C:\Users\serwi> ping -i 14 -n 1 207.172.157.201

Pinging 207.172.157.201 with 32 bytes of data:
Reply from 207.172.157.201: bytes=32 time=115ms TTL=51

Ping statistics for 207.172.157.201:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 115ms, Maximum = 115ms, Average = 115ms
```
### Serwery z zachodniego wybrzeża Ameryki Północnej
#### Serwer z San Francisco (do 27 / od 13)
```powershell
PS C:\Users\serwi> ping -i 27 -n 1 75.144.29.142

Pinging 75.144.29.142 with 32 bytes of data:
Reply from 75.144.29.142: bytes=32 time=180ms TTL=51

Ping statistics for 75.144.29.142:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 180ms, Maximum = 180ms, Average = 180ms
```
#### Serwer z Los Angeles (do 14 / od 14)
```powershell
PS C:\Users\serwi> ping -i 14 -n 1 96.44.135.205

Pinging 96.44.135.205 with 32 bytes of data:
Reply from 96.44.135.205: bytes=32 time=173ms TTL=114

Ping statistics for 96.44.135.205:
    Packets: Sent = 1, Received = 1, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 173ms, Maximum = 173ms, Average = 173ms
```
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
### Tabela z wynikami
| Adres serwera          | Lokalizacja               | Ilość hopów do | Ilość hopów od |
|------------------------|---------------------------|----------------|----------------|
| cs.pwr.edu.pl          | Wrocław                   | 4              | 3              |
| uwr.edu.pl             | Wrocław                   | 11             | 11             |
| spartankebab.eatbu.com | Wrocław                   | 8              | 17             |
| wokin.pl               | Wrocław                   | 8              | 17             |
| put.poznan.pl          | Poznań                    | 8              | 9              |
| amu.edu.pl             | Poznań                    | 9              | 9              |
| avenidapoznan.com      | Poznań                    | 15             | 15             |
| eaiib.agh.edu.pl       | Kraków                    | 10             | 8              |
| ans.konin.pl           | Konin                     | 9              | 14             |
| pb.edu.pl              | Białystok                 | 8              | 7              |
| gminabocki.pl          | Boćki                     | 9              | 14             |
| miasto.hrubieszow.pl   | Hrubieszów                | 16             | 14             |
| fu-berlin.de           | Berlin                    | 16             | 16             |
| cuni.cz                | Praga                     | 15             | 13             |
| vu.lt                  | Wilno                     | 13             | 12             |
| lnu.edu.ua             | Lwów                      | 14             | 15             |
| umontpellier.fr        | Montpellier               | 16             | 14             |
| home.web.cern.ch       | Zurych                    | 22             | 16             |
| aena.es                | Palma de Mallorca         | 19             | 17             |
| visitgibraltar.gi      | Gibraltar                 | 14             | 14             |
| tenki.jp               | Osaka                     | 16             | 15             |
| goo.ne.jp              | Tokyo                     | 23             | 19             |
| vop.co.kr              | Seoul                     | 23             | 23             |
| gov.lk                 | Sri Jayewardenepura Kotte | 20             | 17             |
| jd.com                 | Beijing                   | 21             | 22             |
| 185.66.9.142           | Nowy Jork                 | 16             | 14             |
| 207.172.157.201        | Linthicum Heights         | 14             | 13             |
| 75.144.29.142          | Boulder Creek             | 27             | 13             |
| 96.44.135.205          | Los Angeles               | 14             | 14             |
| wellington.govt.nz     | Wellington                | 36             | 35             |

## Wielkość pakietu
### Limit przesyłania pakietu
Maksymalna wielkość pakietu jaką pozwala nam wysłać `ping` to 65500 bajtów.
```powershell
PS C:\Users\serwi> ping -l 65501 miasto.hrubieszow.pl
Bad value for option -l, valid range is from 0 to 65500.
```
Jak widzimy poniżej, do miasta Hrubieszów możemy wysłać maksymalnie 1472 bajty.
Bierze się ona stąd, że 1500 jest standardową wartością [MTU](https://en.wikipedia.org/wiki/Maximum_transmission_unit).
Po dodaniu 20 bajtów poświęconych na adres IP, i dodatkowo 8 bajtów na nagłówek [ICMP](https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol),
otrzymujemy 8 + 20 + 1472 = 1500 ([tu jest to opisane](https://jasonmurray.org/posts/2020/icmpmtu/)).
```powershell
PS C:\Users\serwi> ping -l 1473 miasto.hrubieszow.pl

Pinging miasto.hrubieszow.pl [54.36.175.33] with 1473 bytes of data:
Request timed out.
Request timed out.
Request timed out.
Request timed out.

Ping statistics for 54.36.175.33:
    Packets: Sent = 4, Received = 0, Lost = 4 (100% loss),
PS C:\Users\serwi> ping -l 1472 miasto.hrubieszow.pl

Pinging miasto.hrubieszow.pl [54.36.175.33] with 1472 bytes of data:
Reply from 54.36.175.33: bytes=1472 time=12ms TTL=53
Reply from 54.36.175.33: bytes=1472 time=13ms TTL=53
Reply from 54.36.175.33: bytes=1472 time=12ms TTL=53
Reply from 54.36.175.33: bytes=1472 time=12ms TTL=53

Ping statistics for 54.36.175.33:
    Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 12ms, Maximum = 13ms, Average = 12ms
```
### Fragmentacja
`ping` nie pozwoli nam przesłać więcej niż 1472 jeśli ustawimy opcję `-f`,
która nie pozwala na fragmentowanie pakietu.
```powershell
PS C:\Users\serwi> ping -l 1473 miasto.hrubieszow.pl -f

Pinging miasto.hrubieszow.pl [54.36.175.33] with 1473 bytes of data:
Packet needs to be fragmented but DF set.
Packet needs to be fragmented but DF set.
Packet needs to be fragmented but DF set.
Packet needs to be fragmented but DF set.

Ping statistics for 54.36.175.33:
    Packets: Sent = 4, Received = 0, Lost = 4 (100% loss),
```
### Wpływ fragmentacji na czas propagacji
Fragmentacja może zwiększyć czas propagacji, co widać w poniższym przykładzie.
Dla niektórych serwerów, nie udało mi się jednak zanotować zauważalnej różnicy.
```powershell
PS C:\Users\serwi> ping archlinux.org -l 65000

Pinging archlinux.org [95.217.163.246] with 65000 bytes of data:
Reply from 95.217.163.246: bytes=65000 time=47ms TTL=50
Reply from 95.217.163.246: bytes=65000 time=47ms TTL=50
Reply from 95.217.163.246: bytes=65000 time=48ms TTL=50
Reply from 95.217.163.246: bytes=65000 time=47ms TTL=50

Ping statistics for 95.217.163.246:
    Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 47ms, Maximum = 48ms, Average = 47ms
```
```powershell
PS C:\Users\serwi> ping archlinux.org -l 30

Pinging archlinux.org [95.217.163.246] with 30 bytes of data:
Reply from 95.217.163.246: bytes=30 time=39ms TTL=50
Reply from 95.217.163.246: bytes=30 time=38ms TTL=50
Reply from 95.217.163.246: bytes=30 time=39ms TTL=50
Reply from 95.217.163.246: bytes=30 time=38ms TTL=50

Ping statistics for 95.217.163.246:
    Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),
Approximate round trip times in milli-seconds:
    Minimum = 38ms, Maximum = 39ms, Average = 38ms
```

## Traceroute (Tracert)
Za pomocą `traceroute` możemy lepiej poznać trasę jaką przechodzi pakiet w drodze
do naszego serwera. Narzędzie to pinguje dany adres wielokrotnie, inkrementując
za każdym razem TTL. Czasami na naszej trasie zauważyć możemy `* * *`, są to serwery,
które nie odpowiedziały na ping od traceroute'a.
```powershell
PS C:\Users\serwi> tracert archlinux.org

Tracing route to archlinux.org [95.217.163.246]
over a maximum of 30 hops:

  1     2 ms     3 ms     2 ms  funbox.home [192.168.1.1]
  2     4 ms     5 ms     3 ms  wro-bng1.neo.tpnet.pl [83.1.4.234]
  3     4 ms     3 ms     3 ms  wro-r11.tpnet.pl [80.50.118.233]
  4     6 ms     5 ms     5 ms  195.116.35.206
  5    16 ms    17 ms    16 ms  hbg-b2-link.ip.twelve99.net [213.248.96.144]
  6    15 ms    16 ms    15 ms  hbg-bb3-link.ip.twelve99.net [62.115.120.70]
  7    33 ms    31 ms    31 ms  s-bb1-link.ip.twelve99.net [62.115.134.95]
  8    38 ms    37 ms    39 ms  hls-b3-link.ip.twelve99.net [62.115.122.33]
  9    38 ms    39 ms    39 ms  hetzner-svc076536-ic365572.ip.twelve99-cust.net [62.115.52.255]
 10    42 ms    38 ms    38 ms  core31.hel1.hetzner.com [213.239.203.205]
 11    39 ms    39 ms    39 ms  spine1.cloud1.hel1.hetzner.com [88.198.249.90]
 12     *        *        *     Request timed out.
 13    40 ms    38 ms    41 ms  17307.your-cloud.host [95.216.135.100]
 14    39 ms    39 ms    40 ms  archlinux.org [95.217.163.246]

Trace complete.
```

## Wireshark
Przy pomocy wiresharka możemy analizować pakiety sieciowe. Często używany jest
do sprawdzania dlaczego dana sieć ma problem z wydajnością. Możemy wtedy przeanalizować
pakiety przychodzące i wychodzące z danej sieci i na tej podstawie określić co leży u sedna problemu.
Ponadto w rękach osób o niecnych zamiarach, można użyc wiresharka do wykradania haseł, od ludzi, którzy
logują się do niezabezpieczonych stron, w sieciach publicznych.
