#include <stdio.h>
#include <stdbool.h>
#include <string.h>

// ? - zgodnosc z dowolnym znakiem
// * - zgodnosc z dowolnym, rowniez pustym ciagiem znakow
// wszystko inne - zgodnosc z samym soba


void debug(char* wzorzec, char* lancuch)
{
    static int debug_iter = 0;
    printf("iteracja nr: %d\n",debug_iter);
    printf("wzorzec = %s\n",wzorzec);
    printf("lancuch = %s\n",lancuch);
    printf("----------------\n");
    debug_iter++;
}

bool match(char* wzorzec, char* lancuch)
{
    //debug(wzorzec,lancuch);

    while(strcmp(wzorzec,"")!=0 && strcmp(lancuch,"")!=0)
    {
        //debug(wzorzec,lancuch);
        if(wzorzec[0] == '?')
        {
            wzorzec++;
            lancuch++;
        }
        else if(wzorzec[0] == '*')
        {
            if(strlen(wzorzec) == 1)
                return true;
            char token = wzorzec[1];
            for(unsigned long k = 0; k < strlen(lancuch); k++)
            {
                if(lancuch[k] == token)
                {
                    if(match(wzorzec + 1, lancuch + k) == true)
                        return true;
                }
            }
            return false;
        }
        else
        {
            if(wzorzec[0] != lancuch[0])
                return false;
            wzorzec++;
            lancuch++;
        }
    }

    return strcmp(wzorzec,lancuch) == 0;
}

int main()
{
    char a[100],b[100],wynik[100];
    scanf("%s %s",a,b);
    strcpy(wynik,(match(a,b) ? (char*)"true" : (char*)"false"));
    printf("match('%s' , '%s') = %s\n",a,b,wynik);
    return 0;
}
