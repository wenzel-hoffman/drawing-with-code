#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type dirname mktemp rm du cut tail time nproc
>/dev/null type ghc runhaskell ffmpeg mpv xdg-open

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR"

TMP_DIR=$(mktemp -d --suffix -ppm-test)

cleanup() (
	>&2 printf '\nCleanup hook…\n\n'
	set -o xtrace
	rm -rfv -- "$TMP_DIR" 2>&1 | tail -n1
)

trap cleanup EXIT

GENERATE_BIN=$TMP_DIR/generate
FRAME_FILE_PATTERN=$TMP_DIR/frame-%09d.ppm
# shellcheck disable=SC2059
FIRST_FRAME_FILE=$(printf "$FRAME_FILE_PATTERN" 0)

CORES=$(nproc --all)

: "${ANIMATION=modified-xordev-shader}"
: "${WIDTH:=600}"
: "${HEIGHT=600}"
: "${FPS:=60}"
: "${DURATION:=10}" # In seconds
: "${THREADS:=$CORES}"

MODE=
if (( $# == 0 )) || ( (( $# == 1 )) && [[ $1 == video ]] ); then
	MODE=video
elif (( $# == 1 )) && [[ $1 == picture ]]; then
	MODE=picture
else
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	>&2 printf '\nUsage: %s (video|picture)\n' "$0"
	exit 1
fi

(
	set -o xtrace
	ghc ./generate.hs -O2 -threaded -rtsopts -with-rtsopts=-N \
		-odir "$TMP_DIR" -hidir "$TMP_DIR" -o "$GENERATE_BIN"
)

case $MODE in
	picture)
		(
			set -o xtrace
			time "$GENERATE_BIN" "$TMP_DIR" "$ANIMATION" "$WIDTH" "$HEIGHT" 1 1 "$THREADS"
			xdg-open "$FIRST_FRAME_FILE"
		)
		;;
	video)
		(
			set -o xtrace
			time "$GENERATE_BIN" "$TMP_DIR" "$ANIMATION" "$WIDTH" "$HEIGHT" "$FPS" "$DURATION" "$THREADS"
		)
		echo -n 'Raw frames size: '
		du -ch "$TMP_DIR"/*.ppm | tail -n1 | cut -d $'\t' -f 1 | cut -d ' ' -f 1
		ANIMATION_FILE_PATH=${ANIMATION}-w-${WIDTH}-h-${HEIGHT}-fps-${FPS}-dur-${DURATION}.mp4
		(
			FFMPEG_CMD=(
				ffmpeg
				-framerate "$FPS"
				-i "$FRAME_FILE_PATTERN"
				-c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p
				"$ANIMATION_FILE_PATH"
			)
			set -o xtrace
			"${FFMPEG_CMD[@]}"
			cleanup
			mpv -- "$ANIMATION_FILE_PATH"
		)
		;;
	*)
		>&2 printf 'Unexpected MODE: %s\n' "$MODE"
		exit 1
esac
