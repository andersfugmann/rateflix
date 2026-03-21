#!/bin/bash
# Test script for fuzzy search with unicode and case normalization
# Reads test cases from test_cases.txt (pipe-delimited)
# Usage: test_fuzzy.sh [server] [port]

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    echo "Usage: $(basename "$0") [server] [port]"
    echo ""
    echo "Run fuzzy search tests against a rateflix server."
    echo ""
    echo "Arguments:"
    echo "  server    Server hostname (default: localhost)"
    echo "  port      Server port (default: 1913)"
    echo ""
    echo "Environment variables:"
    echo "  SERVER_URL  Override full URL (e.g. http://host:port)"
    echo "  SERVER      Server hostname (overridden by argument)"
    echo "  PORT        Server port (overridden by argument)"
    echo "  TEST_FILE   Path to test cases file"
    exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_FILE="${TEST_FILE:-$SCRIPT_DIR/test_cases.txt}"

SERVER="${1:-${SERVER:-localhost}}"
PORT="${2:-${PORT:-1913}}"
SERVER_URL="${SERVER_URL:-http://$SERVER:$PORT}"

PASSED=0
FAILED=0
FAILED_LINES=()
FAILED_TITLES=()
FAILED_YEARS=()
FAILED_TYPES=()
FAILED_EXPECTED=()
FAILED_DESCS=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

TMPDIR_TEST=$(mktemp -d)
trap 'rm -rf "$TMPDIR_TEST"' EXIT

trim() {
    local s="$1"
    s="${s#"${s%%[![:space:]]*}"}"
    s="${s%"${s##*[![:space:]]}"}"
    echo "$s"
}

# Map simple type names to ppx_deriving_yojson JSON format
format_title_types() {
    local types="$1"
    if [ -z "$types" ]; then return; fi
    local result=""
    IFS=',' read -ra PARTS <<< "$types"
    for part in "${PARTS[@]}"; do
        part="$(trim "$part")"
        if [ -n "$result" ]; then
            result="$result,[\"$part\"]"
        else
            result="[\"$part\"]"
        fi
    done
    echo "$result"
}

# Build a JSON query object for one test entry
build_query_json() {
    local title="$1" year="$2" title_types="$3"
    local obj="{\"title\": \"$title\""
    [ -n "$year" ] && obj="$obj, \"year\": $year"
    [ -n "$title_types" ] && obj="$obj, \"title_types\": [$title_types]"
    echo "$obj}"
}

# Build payload from parallel arrays and write to file
build_payload() {
    local -n _titles=$1 _years=$2 _types=$3
    local count=${#_titles[@]}
    local payload="["
    for ((i = 0; i < count; i++)); do
        [ "$i" -gt 0 ] && payload="$payload,"
        payload="$payload$(build_query_json "${_titles[$i]}" "${_years[$i]}" "${_types[$i]}")"
    done
    echo "$payload]"
}

# Check results from a response file against expected values
check_results() {
    local response_file="$1"
    local -n _titles=$2 _years=$3 _types=$4 _expected=$5 _descs=$6
    local count=${#_titles[@]}
    local response
    response=$(<"$response_file")

    for ((i = 0; i < count; i++)); do
        local title="${_titles[$i]}"
        local year="${_years[$i]}"
        local title_types="${_types[$i]}"
        local expected_id="${_expected[$i]}"
        local desc="${_descs[$i]}"

        local imdb_id result_title match_score result_year
        imdb_id=$(echo "$response" | jq -r ".[$i][1].imdb_id")
        result_title=$(echo "$response" | jq -r ".[$i][1].title")
        match_score=$(echo "$response" | jq -r ".[$i][1].match_score")
        result_year=$(echo "$response" | jq -r ".[$i][1].year // \"unknown\"")

        local year_info=""
        [ -n "$year" ] && year_info=" ($year)"
        [ -n "$title_types" ] && year_info="$year_info type:$title_types"

        if [ "$imdb_id" = "$expected_id" ]; then
            echo -e "${GREEN}✓${NC} $desc: '$title'${year_info} → \"$result_title\" ($result_year) score: $match_score"
            ((PASSED++))
        else
            FAILED_LINES+=("${RED}✗${NC} $desc: '$title'${year_info} → \"$result_title\" ($result_year) score: $match_score [expected: $expected_id, got: $imdb_id]")
            FAILED_TITLES+=("$title")
            FAILED_YEARS+=("$year")
            FAILED_TYPES+=("$title_types")
            FAILED_EXPECTED+=("$expected_id")
            FAILED_DESCS+=("$desc")
            ((FAILED++))
        fi
    done
}

echo "=== Fuzzy Search Tests ==="
echo "Server: $SERVER_URL"
echo "Test file: $TEST_FILE"
echo ""

if [ ! -f "$TEST_FILE" ]; then
    echo -e "${RED}Error: Test file not found: $TEST_FILE${NC}"
    exit 1
fi

# --- Parse all sections from the test file ---
NUM_SECTIONS=0
declare -a SECTION_NAMES

flush_section() {
    if [ ${#SEC_TITLES[@]} -eq 0 ]; then return; fi
    local s=$NUM_SECTIONS
    # Save section data into numbered parallel arrays
    eval "S${s}_TITLES=(\"\${SEC_TITLES[@]}\")"
    eval "S${s}_YEARS=(\"\${SEC_YEARS[@]}\")"
    eval "S${s}_TYPES=(\"\${SEC_TYPES[@]}\")"
    eval "S${s}_EXPECTED=(\"\${SEC_EXPECTED[@]}\")"
    eval "S${s}_DESCS=(\"\${SEC_DESCS[@]}\")"
    ((NUM_SECTIONS++))
}

SEC_TITLES=(); SEC_YEARS=(); SEC_TYPES=(); SEC_EXPECTED=(); SEC_DESCS=()

while IFS= read -r line || [ -n "$line" ]; do
    [[ -z "$line" ]] && continue

    if [[ "$line" =~ ^#\ ---\ (.+)\ ---$ ]]; then
        flush_section
        SEC_TITLES=(); SEC_YEARS=(); SEC_TYPES=(); SEC_EXPECTED=(); SEC_DESCS=()
        SECTION_NAMES+=("${BASH_REMATCH[1]}")
        continue
    fi

    [[ "$line" =~ ^# ]] && continue

    IFS='|' read -ra FIELDS <<< "$line"
    title="$(trim "${FIELDS[0]}")"
    expected_id="$(trim "${FIELDS[1]}")"
    year="$(trim "${FIELDS[2]:-}")"
    title_types_raw="$(trim "${FIELDS[3]:-}")"
    desc="$(trim "${FIELDS[4]:-}")"
    [ -z "$desc" ] && desc="$title"
    title_types="$(format_title_types "$title_types_raw")"

    SEC_TITLES+=("$title")
    SEC_YEARS+=("$year")
    SEC_TYPES+=("$title_types")
    SEC_EXPECTED+=("$expected_id")
    SEC_DESCS+=("$desc")
done < "$TEST_FILE"

flush_section

# --- Fire all section requests in parallel ---
for ((s = 0; s < NUM_SECTIONS; s++)); do
    eval "local_titles=(\"\${S${s}_TITLES[@]}\")"
    eval "local_years=(\"\${S${s}_YEARS[@]}\")"
    eval "local_types=(\"\${S${s}_TYPES[@]}\")"
    payload=$(build_payload local_titles local_years local_types)
    curl -s -X POST "$SERVER_URL/lookup" \
        -H "Content-Type: application/json" \
        -d "$payload" \
        -o "$TMPDIR_TEST/response_$s" &
done

wait

# --- Check results in section order ---
for ((s = 0; s < NUM_SECTIONS; s++)); do
    echo ""
    echo "--- ${SECTION_NAMES[$s]} ---"
    eval "local_titles=(\"\${S${s}_TITLES[@]}\")"
    eval "local_years=(\"\${S${s}_YEARS[@]}\")"
    eval "local_types=(\"\${S${s}_TYPES[@]}\")"
    eval "local_expected=(\"\${S${s}_EXPECTED[@]}\")"
    eval "local_descs=(\"\${S${s}_DESCS[@]}\")"
    check_results "$TMPDIR_TEST/response_$s" local_titles local_years local_types local_expected local_descs
done

# Summary
echo ""
echo "=== Results ==="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "=== Failed Tests ==="
    for line in "${FAILED_LINES[@]}"; do
        echo -e "$line"
    done

    echo ""
    echo "=== Re-running failed tests (for server debug output) ==="
    payload=$(build_payload FAILED_TITLES FAILED_YEARS FAILED_TYPES)
    curl -s -X POST "$SERVER_URL/lookup" \
        -H "Content-Type: application/json" \
        -d "$payload" \
        -o "$TMPDIR_TEST/response_rerun"
    check_results "$TMPDIR_TEST/response_rerun" FAILED_TITLES FAILED_YEARS FAILED_TYPES FAILED_EXPECTED FAILED_DESCS
    exit 1
fi
