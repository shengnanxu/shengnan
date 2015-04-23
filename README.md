# shengnan
reuters=read.csv("reutersCSV.csv");
#create function to preprocess the corpus. Note that source.corpus is a corpus to be input.
#it is advised to have this function return into an empty variable. for example: dest.corpus <- mt_preprocess(source.corpus)
  mt_preprocess <- function(source.corpus) {
      temp.corpus <- source.corpus
    	temp.corpus <- tm_map(temp.corpus,removePunctuation) 
    	temp.corpus <- tm_map(temp.corpus,removeNumbers) 
    	temp.corpus <- tm_map(temp.corpus,tolower) 
    	temp.corpus <- tm_map(temp.corpus,removeWords,stopwords("english")) 
    	temp.corpus <- tm_map(temp.corpus,removeWords,"reuters")
    	temp.corpus <- tm_map(temp.corpus,removeWords,"reuter")
    	temp.corpus <- tm_map(temp.corpus,removeWords,"said") 
    	temp.corpus <- tm_map(temp.corpus,stripWhitespace)
    
      	clean.corpus <- temp.corpus
    
      	return(clean.corpus)
    }

  #create function to generate the necessary tdms and matrices for our modelling.
  mt_fulltdm <- function(clean.corpus) {
    
      
  	unigram.tdm <- TermDocumentMatrix(clean.corpus)
  	bigram.tdm <- TermDocumentMatrix(clean.corpus, control = list(tokenize = mt_bigramtokenizer(clean.corpus)))
  	trigram.tdm <- TermDocumentMatrix(clean.corpus, control = list(tokenize = mt_trigramtokenizer(clean.corpus)))
    
      	full.tdm <- rbind(unigram.tdm, bigram.tdm, trigram.tdm)
    	full.tdm <- as.TermDocumentMatrix(full.tdm,weightTfIdf)
    
      	return(full.tdm)
    }

  #create a function to generate a classifier based on a target topic.
  #allow choice between two classifier types bayesian and svm.
  #allow choice of target topic, default to trg.topics outlined above.
  
  mt_fulltdm_label <- function(clean.corpus, source.tdm) {
    #determine target topic
      
      	target.topics <-c("acq","earn","money-fx","grain","crude","trade","interest","ship")
    
      	sfilter.vect <- sFilter(clean.corpus,"LEWISSPLIT == 'TRAIN'")
    	tf.sfilter.vect <- mt_createvect(sfilter.vect,clean.corpus)
    	
      	# originally, this function was intended to cycle through a list of Topics input.
      	# however, tm package seemed unable to support my specific method and I didn't have the resources to bugfix
      	# now it cycles only through the set of Topics recommended by the paper
      	topic.set = c()
    
      	topic.vect <- sFilter(clean.corpus,"Topics == 'acq'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'earn'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'money-fx'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'grain'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'crude'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'trade'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'interest'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    	topic.vect <- sFilter(clean.corpus,"Topics == 'ship'")
    	tf.topic.vect <- mt_createvect(topic.vect,clean.corpus)
    	topic.set <- rbind(topic.set,tf.topic.vect)
    
          rownames(topic.set) <- target.topics
    	topic.set <- as.data.frame(topic.set)
    	
      	i <- 1
    	class.vect <- c()
    	include.vect <- c() 
       for(i in 1:ncol(topic.set)){
              if(TRUE %in% topic.set[,i]){
                	include.vect <- append(include.vect,TRUE)
                	class.vect <- append(class.vect,target.topics[match(TRUE,topic.set[,i])])
                    }
      
                else{
                  	include.vect <- append(include.vect,FALSE)
                  	class.vect <- append(class.vect,FALSE)
          			}
      
            	i <- i+1
          }
    
          topic.set <- rbind (include.vect, class.vect,tf.sfilter.vect )
    
          rownames(topic.set) <- c("Include","Class","Train")
        print("Labels Produced. The next process could take some time, depending on the tdm and corpus.")
     
          i <- match(TRUE,topic.set[1,])
        out.tdm <- source.tdm[,i]
        new.train.set <- c(topic.set[3,i])
        class.set <- c(topic.set[2,i])
    
          i <- i + 1
        q <- ncol(topic.set) + 1
        while(i < q) {
          	if(topic.set[1,i] == TRUE){
            		out.tdm<- append(out.tdm,source.tdm[,i])
            		new.train.set <- append(new.train.set,topic.set[3,i])
            		class.set <- append(class.set,topic.set[2,i])}
          	i <- i + 1
          }
        
          return(list(out.tdm,new.train.set,class.set))}
}

  mt_empty_problem <- function (labels_tdm) {
    
      	ip.1 <- as.DocumentTermMatrix(removeSparseTerms(labels_tdm[1][[1]], 0.99), weightTfIdf)
    	ip.2 <- as.data.frame(cbind(as.data.frame(labels_tdm[2]),as.data.frame(labels_tdm[3])))
    	colnames(ip.2) <- c("Train","Class")
    
      	temp.stm.mat <- ip.1[1,]
        empty.ref.vect <- c(1)
        temp.df <- ip.2[1,]
    
      	i <- 2
    	q <- nrow(ip.1) + 1
    	print(q)
    	print("Lets be hopeful yo")
    	while(i < q){
      		if(sum(ip.1[i,])>0){
        			temp.stm.mat <- rbind(temp.stm.mat,ip.1[i,])
        			empty.ref.vect <- append(empty.ref.vect,i)
        			temp.df <- rbind(temp.df,ip.2[i,])
        		}
      		i <- i + 1
      	print(i)
      	}
    	
      
      
      	op.labels <- temp.df
    	op.dtm <- as.DocumentTermMatrix(temp.stm.mat, weightTfIdf)
    	return(list(op.dtm,op.labels))
    
      }

  #At this point you have a dtm as mt_empty_problem(labels_tdm)[1][[1]] or whichever variavble you passed it to
  #You can use this dtm with weightTfIdf.
  #Then CTM and LDA are both functions that take the form LDA(dtm, k = 2) where k is a targer number of Topics
  #To extract the Topic Models and their terms please see the documentation on the topicmodels package.
  

  
  
  mt_prepforclass_clust <- function(ip.dtm,ip.labels){
    
      	#bear in mind that these inputs are labels_tdm[1] and labels_tdm[2], respecitively
      
      	temp.dtm <- as.DocumentTermMatrix(ip.dtm[[1]], weightTfIdf)
    	temp.labels <- as.data.frame(ip.labels)
    
      	temp.dtm <- as.matrix(temp.dtm)
    	temp.dtm <- as.data.frame(temp.dtm)
    	q <- ncol(temp.dtm) + 1
    	temp.dtm <- cbind(temp.dtm,temp.labels)
    
      	train.set <- temp.dtm[ which(temp.dtm$Train==TRUE),]
    	test.set <- temp.dtm[ which(temp.dtm$Train==FALSE),]
    
      	train.set <- train.set[c(-q)]
    	test.set <- test.set[c(-q)]
    
      	return(list(train.set,test.set))
