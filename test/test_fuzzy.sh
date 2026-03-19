#!/bin/bash
# Test script for fuzzy search with unicode and case normalization
# Reads test cases from test_cases.txt (pipe-delimited)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_FILE="${TEST_FILE:-$SCRIPT_DIR/test_cases.txt}"
SERVER_URL="${SERVER_URL:-http://localhost:8080}"
PASSED=0
FAILED=0
FAILED_LINES=()
FAILED_QUERIES=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
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
    local result_title
    result_title=$(echo "$response" | jq -r '.[0][1].title')
    local match_score
    match_score=$(echo "$response" | jq -r '.[0][1].match_score')
    local result_type
    result_type=$(echo "$response" | jq -r '.[0][1].title_type.[0]')
    local result_year
    result_year=$(echo "$response" | jq -r '.[0][1].year // "unknown"')

    local year_info=""
    if [ -n "$year" ]; then year_info=" ($year)"; fi
    if [ -n "$title_types" ]; then year_info="$year_info type:$title_types"; fi

    if [ "$imdb_id" = "$expected_id" ]; then
        echo -e "${GREEN}✓${NC} $desc: '$title'${year_info} → \"$result_title\" ($result_year) score: $match_score"
        ((PASSED++))
    else
        FAILED_LINES+=("${RED}✗${NC} $desc: '$title'${year_info} → \"$result_title\" ($result_year) score: $match_score [expected: $expected_id, got: $imdb_id]")
        FAILED_QUERIES+=("$title|$expected_id|$year|$title_types|$desc")
        ((FAILED++))
    fi
}

# Map simple type names to ppx_deriving_yojson JSON format
format_title_types() {
    local types="$1"
    if [ -z "$types" ]; then
        echo ""
        return
    fi
    # Convert comma-separated simple names to yojson variant format
    # e.g. "movie" -> "[\"movie\"]"  or "movie,tvSeries" -> "[\"movie\"],[\"tvSeries\"]"
    local result=""
    IFS=',' read -ra PARTS <<< "$types"
    for part in "${PARTS[@]}"; do
        part="${part#"${part%%[![:space:]]*}"}"; part="${part%"${part##*[![:space:]]}"}"  # trim
        if [ -n "$result" ]; then
            result="$result,[\"$part\"]"
        else
            result="[\"$part\"]"
        fi
    done
    echo "$result"
}

echo "=== Fuzzy Search Tests ==="
echo "Server: $SERVER_URL"
echo "Test file: $TEST_FILE"
echo ""

if [ ! -f "$TEST_FILE" ]; then
    echo -e "${RED}Error: Test file not found: $TEST_FILE${NC}"
    exit 1
fi

current_section=""

while IFS= read -r line || [ -n "$line" ]; do
    # Skip blank lines
    [[ -z "$line" ]] && continue

    # Section headers (comments starting with # ---)
    if [[ "$line" =~ ^#\ ---\ (.+)\ ---$ ]]; then
        current_section="${BASH_REMATCH[1]}"
        echo ""
        echo "--- $current_section ---"
        continue
    fi

    # Skip other comments
    [[ "$line" =~ ^# ]] && continue

    # Parse pipe-delimited fields: query | expected_id [| year [| title_types [| description]]]
    IFS='|' read -ra FIELDS <<< "$line"

    # Trim leading/trailing whitespace without xargs (which chokes on quotes)
    trim() { local s="$1"; s="${s#"${s%%[![:space:]]*}"}"; s="${s%"${s##*[![:space:]]}"}"; echo "$s"; }
    title="$(trim "${FIELDS[0]}")"
    expected_id="$(trim "${FIELDS[1]}")"
    year="$(trim "${FIELDS[2]:-}")"
    title_types_raw="$(trim "${FIELDS[3]:-}")"
    desc="$(trim "${FIELDS[4]:-}")"

    # Default description to the title if not provided
    [ -z "$desc" ] && desc="$title"

    # Format title_types for the JSON payload
    title_types="$(format_title_types "$title_types_raw")"

    query "$title" "$year" "$expected_id" "$desc" "$title_types"
done < "$TEST_FILE"

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
    for entry in "${FAILED_QUERIES[@]}"; do
        IFS='|' read -ra F <<< "$entry"
        query "${F[0]}" "${F[2]}" "${F[1]}" "${F[4]}" "${F[3]}"
    done
    exit 1
fi
