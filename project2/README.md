# FLP Projekt: Řešič Rubikovy Kostky (2024/2025)

**Autor:** Jakub Vlk
**Login:** xvlkja07

## Popis Projektu

Tento projekt implementuje řešič pro standardní 3x3x3 Rubikovu kostku v jazyce SWI-Prolog. Cílem je načíst zamíchaný stav kostky ze standardního vstupu a vypsat sekvenci tahů, které vedou k jejímu složení (každá strana má jednu barvu).

## Implementace

### Reprezentace Kostky

Stav kostky je interně reprezentován jako seznam šesti seznamů, kde každý vnitřní seznam odpovídá jedné stěně kostky a obsahuje 9 čísel (barev). Pořadí stěn v hlavním seznamu je:

1.  **UP** (Horní)
2.  **FRONT** (Přední)
3.  **RIGHT** (Pravá)
4.  **BACK** (Zadní)
5.  **LEFT** (Levá)
6.  **DOWN** (Spodní)

Číslování políček na každé stěně je standardní (1-9, po řádcích).

### Načítání Vstupu

Program čte stav kostky ze standardního vstupu v předepsaném formátu:
- 3 řádky pro horní stěnu (UP)
- 3 řádky pro střední pás (L F R B) - formát `LLL FFF RRR BBB`
- 3 řádky pro spodní stěnu (DOWN)

Vstup je parsován pomocí vestavěných predikátů `read_line_to_string/2`, `split_string/4`, `string_codes/2`, `maplist/2` a pomocných predikátů pro převod na číselnou reprezentaci.

### Metoda Řešení

Hlavní metodou použitou pro nalezení řešení je **Iterative Deepening Search (IDS)** implementovaná predikátem `solve_ids/2`.

### Cílový Stav

Cílový (vyřešený) stav je definován predikátem `is_solved/1`, který kontroluje, zda má každá ze šesti stěn kostky pouze jednu barvu (všechna políčka na stěně mají stejné číslo). Přesná orientace barev ve složeném stavu není pevně daná (např. UP nemusí být nutně barva 1).

### Vstupy
Vstupy na kterých jsem testoval:

| Soubory                 |  Čas   | Počet tahů pro zamíchaní  |
|-------------------------|--------|---------------------------|
| cube_len6_3.txt         | 0.02s  | 6                         |
| cube_len7_1.txt         | 23,87s | 7                         |
| cube_len8_1.txt         | 2:23s  | 8                         |

*Tyto vstupy jsou "štastné" a nereperezentují schopnosti řešit obecné kostky.
## Návod k Použití

### Překlad

Projekt obsahuje `Makefile`. Pro překlad použijte příkaz:
```bash
make