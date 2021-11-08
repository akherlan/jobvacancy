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

Awalnya mau dibuat sebuah bot yang bekerja 24/7 untuk forward pesan lowongan kerja dari [grup](https://t.me/gnurindonesia/) untuk dipisahkan ke channel khusus. Tapi akhirnya rencana ini disimpan dulu untuk rencana jangka panjang saja dengan fungsi yang mungkin bisa diperkaya, alias tidak hanya untuk menyimpan lowongan kerja di Channel saja. Karena sepertinya grup belum perlu-perlu amat punya fitur auto-forward pesan ke [channel](https://t.me/gnuridjobs/), setidaknya untuk saat ini. Mungkin kalau nanti ada kulgram fitur itu akan cocok dan bisa sangat bermanfaat.

Mari kita jadikan worker saja untuk alat semi-otomatis forward lowongan kerja ke channel dari berbagai sumber. Di sini tugas Admin untuk supervisi pesan tetap dibutuhkan. Manusia memang selalu lebih baik.

### Progres

-   [x] Format postingan
-   [x] Limit query
-   [x] Filter kualifikasi/skill
-   [x] Simpan hasil, cek apakah dobel
-   [x] Forward Telegram
-   [ ] Github Action: cron/automation
-   [ ] Buat proses mining tetap memproses hasil walau gagal di tengah jalan
-   [ ] Post feature enrichment: full/part-time? is remote? tag?

### Tautan

Telegram group: [Komunitas R Indonesia](https://t.me/GNURIndonesia)  
Telegram channel lowongan R: [\@gnuridjobs](https://t.me/gnuridjobs)   
Telegram channel feed artikel R: [\@feed_r](https://t.me/feed_r)
