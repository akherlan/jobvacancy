## Semi-automatic Job Vacancies Worker

**For Telegram Channel and Personal Chat Feeder**

Untuk kemudahan supaya gak bolak-balik cek situs web lowongan kerja.

Data bersumber dari:

- Glints
- Jobstreet
- Indeed (coming soon)

### Alur kerja

- Menyortir lowongan kerja
- Memformat konten postingan loker: judul, deskripsi, tautan
- Mengirim konten loker ke personal chat (admin) untuk supervisi konten
- Meneruskan postingan ke Telegram channel

### Motivasi

Awalnya mau dibuat sebuah bot yang bekerja 24/7 untuk forward pesan lowongan kerja dari [Telegram group](https://t.me/gnurindonesia/) agar terpisah ke [Telegram channel](https://t.me/gnuridjobs/) khusus. Tapi akhirnya rencana ini disimpan dulu untuk rencana jangka panjang saja dengan fungsi yang mungkin bisa diperkaya, alias tidak hanya untuk menyimpan lowongan kerja di Channel saja. Karena sepertinya grup belum perlu-perlu amat punya fitur auto-forward pesan ke channel, setidaknya untuk saat ini. Mungkin kalau nanti ada kulgram fitur itu akan cocok dan bisa sangat bermanfaat.

Mari kita jadikan bot ini sebagai worker saja untuk alat semi-otomatis forward lowongan kerja ke channel dari berbagai sumber. Di sini tugas Admin untuk supervisi pesan tetap dibutuhkan. Manusia memang selalu lebih baik.

### Progres

-   [x] Format postingan
-   [x] Limit query
-   [x] Filter kualifikasi/skill
-   [x] Simpan hasil, cek apakah dobel
-   [x] Forward Telegram
-   [x] Github Action: cron/automation
-   [x] Buat proses mining tetap melanjutkan step walau gagal di tengah jalan
-   [ ] Gagal mining maka data scraped/posted jangan dulu disimpan
-   [ ] Post feature enrichment: full/part-time? is remote? tag?

### Tautan

Telegram group: [Komunitas R Indonesia](https://t.me/GNURIndonesia)  
Telegram channel lowongan R: [\@gnuridjobs](https://t.me/gnuridjobs)   
Telegram channel feed artikel R: [\@feed_r](https://t.me/feed_r)
