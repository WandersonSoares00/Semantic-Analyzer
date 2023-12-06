input="$1"

ocamlopt Term.ml main.ml

while IFS= read -r linha; do
    ./a.out <<< "$linha"
done < "$input"
