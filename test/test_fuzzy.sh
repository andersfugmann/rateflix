#!/bin/bash
# Test script for fuzzy search with unicode and case normalization


SERVER_URL="${SERVER_URL:-http://localhost:8080}"
PASSED=0
FAILED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

query() {
    local title="$1"
    local year="$2"
    local expected_id="$3"
    local desc="$4"
    local title_types="$5"

    if [ -n "$title_types" ]; then
        if [ -n "$year" ]; then
            local payload="[{\"title\": \"$title\", \"year\": $year, \"title_types\": [$title_types]}]"
        else
            local payload="[{\"title\": \"$title\", \"title_types\": [$title_types]}]"
        fi
    elif [ -n "$year" ]; then
        local payload="[{\"title\": \"$title\", \"year\": $year}]"
    else
        local payload="[{\"title\": \"$title\"}]"
    fi

    local response
    response=$(curl -s -X POST "$SERVER_URL/lookup" \
        -H "Content-Type: application/json" \
        -d "$payload")

    local imdb_id
    imdb_id=$(echo "$response" | jq -r '.[0][1].imdb_id')
    local match_score
    match_score=$(echo "$response" | jq -r '.[0][1].match_score')
    local result_type
    result_type=$(echo "$response" | jq -r '.[0][1].title_type.[0]')
    local result_year
    result_year=$(echo "$response" | jq -r '.[0][1].year // "unknown"')

    if [ "$imdb_id" = "$expected_id" ]; then
        echo -e "${GREEN}✓${NC} $desc"
        echo "  Query: '$title' → $imdb_id (year: $result_year, score: $match_score, type: $result_type)"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} $desc"
        echo "  Query: '$title'"
        echo "  Expected: $expected_id, Got: $imdb_id (year: $result_year, score: $match_score, type: $result_type)"
        echo "$response" | jq || echo $response
        ((FAILED++))
    fi
}

echo "=== Fuzzy Search Tests ==="
echo "Server: $SERVER_URL"
echo ""

# Test unicode normalization - accented characters
echo "--- Unicode Accented Characters ---"
query "Amélie" 2001 "tt0211915" "French é accent"
query "Amelie" 2001 "tt0211915" "Without accent (should match)"
query "Léon: The Professional" 1994 "tt0110413" "French é with colon"
query "Leon The Professional" 1994 "tt0110413" "Without accent or colon"
query "The City of Lost Children" 1995 "tt0112682" "English title for French film"

# Test case insensitivity
echo ""
echo "--- Case Insensitivity ---"
query "THE GODFATHER" 1972 "tt0068646" "All uppercase"
query "the godfather" 1972 "tt0068646" "All lowercase"
query "The Godfather" 1972 "tt0068646" "Title case"
query "AMÉLIE" 2001 "tt0211915" "Uppercase with accent"
query "amélie" 2001 "tt0211915" "Lowercase with accent"

# Test unicode characters in international titles
echo ""
echo "--- International Unicode Titles ---"
query "Nausicaä of the Valley of the Wind" 1984 "tt0087544" "Diaeresis (ä)"
query "Nausicaa of the Valley of the Wind" 1984 "tt0087544" "Without diaeresis"
query "Hôtel Rwanda" 2004 "tt0395169" "French ô"
query "Hotel Rwanda" 2004 "tt0395169" "Without accent"
query "Koyaanisqatsi" 1982 "tt0085809" "Hopi title no accents"
query "KOYAANISQATSI" 1982 "tt0085809" "Uppercase variant"

# Test special characters and punctuation normalization
echo ""
echo "--- Punctuation Handling ---"
query "Who's Afraid of Virginia Woolf?" 1966 "tt0061184" "Apostrophe and question mark"
query "Whos Afraid of Virginia Woolf" 1966 "tt0061184" "Without punctuation"
query "Schindler's List" 1993 "tt0108052" "Apostrophe in title"
query "Schindlers List" 1993 "tt0108052" "Without apostrophe"

# Test year disambiguation - same title, different years
echo ""
echo "--- Year Disambiguation ---"
query "Singin' in the Rain" 1952 "tt0045152" "Classic 1952 musical"
query "Singin in the Rain" 1952 "tt0045152" "1952 without apostrophe"
query "A Star Is Born" 1937 "tt0029606" "1937 original"
query "A Star Is Born" 1954 "tt0047522" "1954 Judy Garland version"
query "A Star Is Born" 1976 "tt0075265" "1976 Barbra Streisand version"
query "The Fly" 1958 "tt0051622" "1958 original"
query "The Fly" 1986 "tt0091064" "1986 Cronenberg remake"
query "Dune" 1984 "tt0087182" "1984 David Lynch version"
query "West Side Story" 1961 "tt0055614" "1961 original"
query "West Side Story" 2021 "tt3581652" "2021 Spielberg remake"
query "Little Women" 1933 "tt0024264" "1933 version"
query "Little Women" 1994 "tt0110367" "1994 Winona Ryder version"
query "Ocean's Eleven" 1960 "tt0054135" "1960 Rat Pack version"
query "Ocean's Eleven" 2001 "tt0240772" "2001 Clooney version"

# Test title_types filter
echo ""
echo "--- Title Type Filtering ---"
query "Breaking Bad" "" "tt0903747" "TV series without filter" '["tvSeries"]'
query "Breaking Bad" "" "tt0903747" "TV series explicit filter" '["tvSeries"]'
query "Fargo" 1996 "tt0116282" "Fargo movie (1996)" '["movie"]'
query "Fargo" "" "tt2802850" "Fargo TV series" '["tvSeries"]'
query "Dune: Part One" 2021 "tt1160419" "Dune movie with type filter" '["movie"]'
query "A Star Is Born" 2018 "tt1517451" "A Star Is Born movie 2018" '["movie"]'
query "Westworld" "" "tt0475784" "Westworld series vs movie" '["tvSeries"]'
query "True Grit" 2010 "tt1403865" "True Grit movie remake" '["movie"]'
query "True Grit" 1969 "tt0065126" "True Grit original movie" '["movie"]'
query "The Fly" 1986 "tt0091064" "The Fly movie" '["movie"]'

# Test very short titles
echo ""
echo "--- Short Titles ---"
query "It" 2017 "tt1396484" "It (2017 horror)" '["movie"]'
query "Us" 2019 "tt6857112" "Us (2019 horror)" '["movie"]'
query "Up" 2009 "tt1049413" "Up (2009 Pixar)" '["movie"]'
query "Her" 2013 "tt1798709" "Her (2013 Spike Jonze)" '["movie"]'
query "Jaws" 1975 "tt0073195" "Jaws (1975)" '["movie"]'
query "M" 1931 "tt0022100" "M (1931 Fritz Lang)" '["movie"]'

# Summary
echo ""
echo "=== Results ==="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
