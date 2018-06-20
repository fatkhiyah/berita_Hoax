library(caret)
library(tm)
library(SnowballC)
library(arm)
# Training data.
data <- c('Butuh 10 Tahun Pulihkan Pulau Dewata Pasca-Bom Bali', 'Korban Selamat Berkeringat Saat Ceritakan Bom Bali','HISTORIPEDIA: Peristiwa Bom Bali, Bencana bagi Indonesia','Jalan Panjang Ali Imron Bom Bali dan Pilihan Jihad','11 Korban Bom Bali Dapat Bantuan','Dubes Australia Berdoa di Monumen Bom Bali','Korban Bom Bali Menjerit Haknya Masih Terabaikan','Keluarga Korban Kenang Peristiwa Bom Bali I','Peringatan Bom Bali I, Tamu Harus Lewati 5 Pemeriksaan','Densus 88 Jaga Ketat Lokasi Peringatan Bom Bali',
'Bom bali tidak ada korban jiwa','Pelaku bom Bali dibebaskan tanpa syarat','Motif peledakan Bom Bali adalah uang','Akibat dari Bom Bali banyak wilayah di tanah air yang terkena teror bom juga','Presiden bantah pelaku peledakan bom bali adalah WNI','Pelaku peledakan bom bali adalah seorang mahasiswa','Sasaran peledakan bom bali adalah WNI','Pelaku bom bali lebih dari 100 orang','Bom bali di ledakkan di tengah laut','Tidak ada penanganan khusus terhadap korban peledakan bom bali')
corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, c(0, 1))
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)
data
train
# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
data2 <- c('Bom Bali menjadi Bencana bagi Indonesia','Ali Imron Bom Bali dan Pilihan Jihad ','Korban Bom Bali Menjerit Haknya Masih Terabaikan','Keluarga Korban Kenang Peristiwa Bom Bali','Peringatan Bom Bali I, Tamu Harus Lewati 5 Pemeriksaan',
'Pelaku bom bali lebih dari 100 orang','Bom bali di ledakkan di tengah laut','Pelaku peledakan bom bali adalah mahasiswa','Motif peledakan Bom Bali adalah uang','Bom bali tidak ada korban jiwa')
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)

