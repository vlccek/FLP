#!/bin/bash

# --- Konfigurace ---
PYTHON_SCRIPT="rubiks.py"
PROLOG_SOLVER="./flp25-log" # Tvůj zkompilovaný Prolog program
MAX_SCRAMBLE_LENGTH=8      # Maximální délka zamíchání k testování
MULTI_INSTANCE_THRESHOLD=6 # Délka, od které generovat více instancí
INSTANCES_PER_LENGTH=3     # Počet instancí pro delší zamíchání
TIMEOUT_DURATION="10m"     # Časový limit pro Prolog řešič (10 minut)
OUTPUT_DIR="cubes"         # Adresář pro uložení úspěšně vyřešených vstupů
PYTHON_CMD="python3"       # Nebo "python"
MAX_JOBS=$(nproc)          # Počet paralelních úloh (počet jader CPU)

# --- Kontrola existence souborů a nástrojů ---
# (Kontroly pro python, prolog solver, timeout, parallel)
command -v $PYTHON_CMD >/dev/null 2>&1 || { echo >&2 "Error: Python '$PYTHON_CMD' nenalezen."; exit 1; }
[ -f "$PYTHON_SCRIPT" ] || { echo >&2 "Error: Python skript '$PYTHON_SCRIPT' nenalezen."; exit 1; }
[ -f "$PROLOG_SOLVER" ] || { echo >&2 "Error: Prolog řešič '$PROLOG_SOLVER' nenalezen (není zkompilovaný?)."; exit 1; }
[ -x "$PROLOG_SOLVER" ] || { echo >&2 "Error: Prolog řešič '$PROLOG_SOLVER' není spustitelný (chmod +x?)."; exit 1; }
command -v timeout >/dev/null 2>&1 || { echo >&2 "Error: Příkaz 'timeout' nenalezen (vyžaduje coreutils)."; exit 1; }
command -v parallel >/dev/null 2>&1 || { echo >&2 "Error: Příkaz 'parallel' nenalezen (vyžaduje GNU Parallel)."; exit 1; }
command -v time >/dev/null 2>&1 || { echo >&2 "Error: Příkaz 'time' nenalezen."; exit 1; } # /usr/bin/time

# --- Vytvoření výstupního adresáře ---
mkdir -p "$OUTPUT_DIR" || { echo >&2 "Error: Nelze vytvořit výstupní adresář '$OUTPUT_DIR'."; exit 1; }

# --- Vytvoření dočasného adresáře pro vstupy ---
TEMP_DIR=$(mktemp -d)
# Past pro úklid dočasného adresáře při ukončení skriptu
trap 'rm -rf -- "$TEMP_DIR"' EXIT

echo "Zahajuji testování..."
echo "Výstupní adresář pro úspěšné vstupy: $OUTPUT_DIR"
echo "Časový limit pro řešič: $TIMEOUT_DURATION"
echo "Maximální počet paralelních úloh: $MAX_JOBS"
echo "========================================"

# --- Funkce, kterou bude spouštět GNU Parallel pro každý vstupní soubor ---
run_solver() {
    local input_file="$1"
    local solver="$2"
    local timeout_dur="$3"
    local output_dir="$4"
    local base_name # Bude nastavena níže

    # Získání base_name bez cesty a přípony
    base_name=$(basename "$input_file" .txt)

    local time_file
    local solver_stdout_stderr_file
    time_file=$(mktemp)
    solver_stdout_stderr_file=$(mktemp)

    # Použijeme /usr/bin/time pro konzistentní formát výstupu času
    # Formát -f '%e' dává elapsed real time v sekundách s desetinnou částí
    # Nebo -f '%E' dává H:MM:SS.ms
    # Nebo zachytíme standardní výstup time a parsujeme 'real'
    local time_output
    # Spustíme časovaný řešič
    ( time -p timeout "$timeout_dur" "$solver" < "$input_file" > "$solver_stdout_stderr_file" 2>&1 ) 2> "$time_file"
    local exit_status=$? # Návratový kód příkazu timeout

    # Zpracování výstupu času - hledáme řádek "real"
    # Formát se může lišit, toto je běžný pro /usr/bin/time -p
    local elapsed_time_str
    elapsed_time_str=$(grep '^real' "$time_file" | awk '{print $2}') # Získá jen číslo sekund
    # Převod sekund na MM:SS.ms (přibližně)
    if [[ -n "$elapsed_time_str" ]]; then
      local total_seconds
      total_seconds=$(LC_NUMERIC=C printf "%.0f" "$elapsed_time_str") # Zaokrouhlení na celé sekundy
      local minutes=$(( total_seconds / 60 ))
      local seconds=$(( total_seconds % 60 ))
      local milliseconds
      milliseconds=$(echo "$elapsed_time_str" | sed 's/.*\.\([0-9]*\).*/\1000/' | cut -c1-3 ) # Získání ms
      local formatted_time
      formatted_time=$(printf "%02d:%02d.%s" "$minutes" "$seconds" "$milliseconds")
    else
      formatted_time="?:??.???" # Pokud se čas nepodařilo získat
    fi


    # Vyhodnocení výsledku
    if [ "$exit_status" -eq 0 ]; then
        # Úspěch
        local final_filename="${output_dir}/${base_name}.txt"
        # Přesuneme dočasný vstupní soubor na finální místo
        mv "$input_file" "$final_filename"
        # Vypíšeme požadovaný formát
        echo "$final_filename - time: $formatted_time"
    elif [ "$exit_status" -eq 124 ]; then
        # Timeout
        echo "$base_name - TIMEOUT ($timeout_dur)" >&2 # Výpis chyby na stderr
        rm "$input_file" # Smažeme neúspěšný vstup
    else
        # Jiná chyba
        echo "$base_name - FAILED (Code: $exit_status)" >&2 # Výpis chyby na stderr
        # Volitelně vypsat obsah solver_stdout_stderr_file pro ladění
        # cat "$solver_stdout_stderr_file" >&2
        rm "$input_file" # Smažeme neúspěšný vstup
    fi

    # Úklid dočasných souborů
    rm "$time_file" "$solver_stdout_stderr_file"
}

# Exportujeme funkci a proměnné pro GNU Parallel
export -f run_solver
export PROLOG_SOLVER TIMEOUT_DURATION OUTPUT_DIR

# --- Smyčka testování délek ---
for (( len=1; len<=MAX_SCRAMBLE_LENGTH; len++ )); do
    echo # Prázdný řádek pro oddělení
    echo "--- Generuji a spouštím pro délku zamíchání: $len ---"

    num_instances=1
    if [ "$len" -ge "$MULTI_INSTANCE_THRESHOLD" ]; then
        num_instances=$INSTANCES_PER_LENGTH
    fi

    input_files=() # Pole pro uložení cest k dočasným vstupním souborům

    # --- Smyčka generování instancí ---
    for (( instance=1; instance<=num_instances; instance++ )); do
        # Vygeneruj vstup
        cube_input=$($PYTHON_CMD "$PYTHON_SCRIPT" -g "$len")
        if [ $? -ne 0 ] || [ -z "$cube_input" ]; then
            echo "CHYBA: Selhalo generování vstupu instance $instance pro délku $len." >&2
            continue # Přeskoč tuto instanci
        fi

        # Vytvoř dočasný vstupní soubor
        # Použijeme predikovatelnější jméno pro base_name ve funkci run_solver
        temp_input_file=$(mktemp "$TEMP_DIR/cube_len${len}_inst${instance}_XXXXXX.txt")
        echo "$cube_input" > "$temp_input_file"
        input_files+=("$temp_input_file") # Přidej soubor do pole
    done

    # --- Spuštění paralelního zpracování pro danou délku ---
    if [ ${#input_files[@]} -gt 0 ]; then
        printf "%s\n" "${input_files[@]}" | parallel -j "$MAX_JOBS" --env PROLOG_SOLVER --env TIMEOUT_DURATION --env OUTPUT_DIR run_solver {} "$PROLOG_SOLVER" "$TIMEOUT_DURATION" "$OUTPUT_DIR"
    else
         echo "Žádné platné vstupy nebyly vygenerovány pro délku $len."
    fi

    # Není potřeba explicitní 'wait' zde, 'parallel' počká na dokončení svých úloh

    # (Shrnutí pro délku už není tak relevantní, protože výstup je okamžitý)

done # Konec smyčky délek

echo
echo "========================================"
echo "Testování dokončeno."

# Úklid bude proveden pomocí 'trap' na EXIT
exit 0