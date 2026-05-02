#!/bin/bash

# Set VLC as the default app for common audio and video file types.
# Requires: brew install duti

set -e

if ! command -v duti >/dev/null 2>&1; then
    echo "duti not found. Install with: brew install duti"
    exit 1
fi

VLC_BUNDLE_ID="org.videolan.vlc"

AUDIO_UTIS=(
    public.mp3
    public.aac-audio
    public.aifc-audio
    public.aiff-audio
    public.audio
    public.mpeg-4-audio
    com.apple.m4a-audio
    com.apple.protected-mpeg-4-audio
    com.microsoft.waveform-audio
    org.xiph.flac
    org.xiph.ogg-audio
)

VIDEO_UTIS=(
    public.mpeg
    public.mpeg-2-video
    public.mpeg-4
    public.movie
    public.video
    public.avi
    public.3gpp
    com.apple.quicktime-movie
    org.matroska.mkv
    org.webmproject.webm
)

AUDIO_EXTS=(mp3 m4a aac wav flac aiff aif ogg oga opus wma)
VIDEO_EXTS=(mp4 m4v mov avi mkv webm flv wmv mpg mpeg 3gp ts)

for uti in "${AUDIO_UTIS[@]}" "${VIDEO_UTIS[@]}"; do
    duti -s "$VLC_BUNDLE_ID" "$uti" all 2>/dev/null && echo "[OK] $uti" || echo "[SKIP] $uti"
done

for ext in "${AUDIO_EXTS[@]}" "${VIDEO_EXTS[@]}"; do
    duti -s "$VLC_BUNDLE_ID" ".$ext" all 2>/dev/null && echo "[OK] .$ext" || echo "[SKIP] .$ext"
done
