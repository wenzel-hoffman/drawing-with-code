#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type dirname mktemp mkdir rm du cut tail time nproc
>/dev/null type cabal ffmpeg mpv feh magick

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR"

TMP_DIR=$(mktemp -d --suffix -ppm-test)

cleanup() (
	>&2 printf '\nCleanup hook…\n\n'
	set -o xtrace
	rm -rfv -- "$TMP_DIR" 2>&1 | tail -n1
)

trap cleanup EXIT

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

: "${RENDERS_DIR:=renders}"

show-usage() {
	set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
	printf 'Usage: %s (video|picture) (save)\n' "$0"
}

MODE=
if (( $# == 0 )) || ( (( $# >= 1 )) && [[ $1 == video ]] ); then
	MODE=video
	if (( $# >= 1 )); then shift; fi
elif (( $# >= 1 )) && [[ $1 == picture ]]; then
	MODE=picture
	shift
else
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	>&2 echo
	>&2 show-usage
	exit 1
fi

SAVE=false
if (( $# == 1 )) && [[ $1 == save ]]; then
	SAVE=true
elif (( $# >= 1 )); then
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	>&2 echo
	>&2 show-usage
	exit 1
fi

case $MODE in
	picture)
		(
			(
				set -o xtrace
				time cabal build -- "$ANIMATION"
				time cabal run -- "$ANIMATION" "$TMP_DIR" "$WIDTH" "$HEIGHT" 1 1 1
			)
			if "$SAVE"; then
				(
					mkdir -p -- "$RENDERS_DIR"
					PNG_FILE=${RENDERS_DIR}/${ANIMATION}-w-${WIDTH}-h-${HEIGHT}.png
					MAGICK_CMD=(
						magick "$FIRST_FRAME_FILE"
						-define png:compression-level=9
						-define png:compression-strategy=2
						-define png:exclude-chunk=all
						"$PNG_FILE"
					)
					set -o xtrace
					"${MAGICK_CMD[@]}"
					feh --scale-down -- "$PNG_FILE"
				)
			else
				(set -o xtrace; feh --scale-down -- "$FIRST_FRAME_FILE")
			fi
		)
		;;
	video)
		(
			set -o xtrace
			time cabal build -- "$ANIMATION"
			time cabal run -- "$ANIMATION" "$TMP_DIR" "$WIDTH" "$HEIGHT" "$FPS" "$DURATION" "$THREADS"
		)
		echo -n 'Raw frames size: '
		du -ch "$TMP_DIR"/*.ppm | tail -n1 | cut -d $'\t' -f 1 | cut -d ' ' -f 1
		mkdir -p -- "$RENDERS_DIR"
		ANIMATION_FILE_PATH=${RENDERS_DIR}/${ANIMATION}-w-${WIDTH}-h-${HEIGHT}-fps-${FPS}-dur-${DURATION}.mp4
		(
			FFMPEG_CMD=(
				ffmpeg
				-r "$FPS"
				-i "$FRAME_FILE_PATTERN"
				-c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p
				"$ANIMATION_FILE_PATH"
			)
			set -o xtrace
			"${FFMPEG_CMD[@]}"
			cleanup
			mpv --video-unscaled=yes --window-scale=1.0 -- "$ANIMATION_FILE_PATH"
		)
		;;
	*)
		>&2 printf 'Unexpected MODE: %s\n' "$MODE"
		exit 1
esac
