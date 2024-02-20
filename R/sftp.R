################################################################################
## LOGIN CREDENTIALS
################################################################################

#' Set SFTP Login credentials
#'
#' De instellingen voor de sftp-login credentials worden opgeslagen in de opties
#' van de sessie. Deze opties worden gebruikt voor de sftp functies.
#'
#' @param username De gebruikersnaam om in te loggen op de server
#' @param server Het adres van de server die benaderd moet worden (bijvoorbeeld: "server.domein.nl")
#' @param ssh.public.keyfile De locatie van het publieke ssh-keyfile
#' @param ssh.private.keyfile De locatie van het prive ssh-keyfile
#' @export
sftp_login_credentials_set <- function(username,
                                       server,
                                       ssh.public.keyfile,
                                       ssh.private.keyfile) {
  ## Haal het vunet-id uit het tekstbestand
  ## Bepaal de credentials
  credentials <- list(
    username = username,
    server = server,
    .opts = list(
      ssh.public.keyfile  = ssh.public.keyfile,
      ssh.private.keyfile = ssh.private.keyfile
    )
  )
  options(
    list(
      VUStudentAnalytics.sftp.credentials = credentials
    )
  )
}


#' Get SFTP Login credentials
#'
#' De instellingen voor de sftp-login credentials worden uit de opties
#' van de sessie gehaald. Deze opties kunnen worden gebruikt voor de sftp functies.
#'
sftp_login_credentials_get <- function() {
  getOption("VUStudentAnalytics.sftp.credentials")
}




################################################################################
## LIST DIRECTORY
################################################################################

#' SFTP list directory
#'
#' Een functie om de bestandsnamen van een map op een sftp server te verkrijgen.
#' Alle bestanden in een folder worden opgehaald en in een vector geretourneerd.
#' Dit is een helper functie die wordt gebruikt binnen sftp_vu_list_dir.
#' @param remote_path Het pad van de folder op de server (exclusief server url)
#' @param server het adres van de server, zonder protocol ervoor, en zonder slash
#' op het einde. (dus exclusief "sftp://").
#' @param username De gebruikersnaam die gebruikt wordt voor authenticatie op de server.
#' @param .opts Een named lijst met opties die meegegeven worden aan de getBinaryURL
#' functie. De opties die voor ssh authenticatie meegegeven moeten worden zijn
#' "ssh.public.keyfile" en "ssh.private.keyfile".
#' @return Een vector met de paden van alle bestanden in de opgegeven folder.
sftp_list_dir <- function(remote_path, server, username, .opts) {
  ## Controleer of RCurl is geinstalleerd
  check_installed_package("RCurl")
  ## Bepaal het sftp pad van de opgegeven server. De username moet in de url
  ## staan
  server_path <- paste0(
    "sftp://",
    username,
    "@",
    server
  )

  ## Bepaal de URL van de map
  dir_url <- paste(server_path, remote_path, sep = "/")

  ## Maak een lijst van alle bestanden in de map
  file_list <-
    utils::read.table(
      textConnection(RCurl::getURL(
        dir_url,
        .opts = .opts,
        dirlistonly = TRUE,
        verbose = F
      )),
      col.names = "file",
      sep = "\n",
      strip.white = TRUE,
      stringsAsFactors = F
    )

  ## Verwijder "." en ".." uit de lijst, en maak een vector van de tabel
  file_list <- file_list[!file_list$file %in% c(".", ".."), ]

  ## Maak een lijst met alle paden en retoureer deze
  ## Als de lijst met bestanden bestanden bevat (dan is de lijst langer dan 0),
  ## zet dan het pad voor de bestandsnaam
  if (length(file_list) > 0) {
    path_list <- paste0(remote_path, file_list)
  } else {
    ## Als de lijst bestanden leeg is, moet de path_list ook leeg zijn.
    path_list <- file_list
  }

  return(path_list)
}

#' SFTP VU list directory
#'
#' Een functie om de bestandsnamen van een map op een sftp server te verkrijgen.
#' Alle bestanden in een folder worden opgehaald en in een vector geretourneerd.
#' Het serveradres en de gebruikersnaam van StudentAnalytics zijn hier al ingevoerd.
#' De functie gaat er van uit dat de public en private ssh key op deze locaties staan:
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa.pub
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa
#' @param remote_path Het pad van het bestand op de server (exclusief server url
#' "Studentanalytics.sftp.vu.nl")
#' @return Een vector met de paden van alle bestanden in de opgegeven folder.
#' @export
sftp_vu_list_dir <- function(remote_path) {
  ## Haal de login credentials van de VU-sftp server op
  sftp_vu_login <- sftp_login_credentials_get()

  ## voer de functie sftp_download uit met de VU gegevens.
  dir_list <- sftp_list_dir(
    remote_path = remote_path,
    server      = sftp_vu_login$server,
    username    = sftp_vu_login$username,
    .opts       = sftp_vu_login$.opts
  )

  return(dir_list)
}







################################################################################
## DOWNLOAD
################################################################################

#' SFTP Download
#'
#' Functie om bestanden te downloaden van een remote sftp server. Dit is een helper
#' functie, die gebruikt wordt in sftp_vu_download.
#' @param remote_path Het pad van het bestand op de server (exclusief server url)
#' @param server Het adres van de server, zonder protocol ervoor, en zonder slash op het einde. (dus exclusief "sftp://")
#' @param username De gebruikersnaam die gebruikt wordt voor authenticatie op de server.
#' @param .opts Een named lijst met opties die meegegeven worden aan de getBinaryURL
#' functie. De opties die voor ssh authenticatie meegegeven moeten worden zijn
#' "ssh.public.keyfile" en "ssh.private.keyfile".
sftp_download <- function(remote_path, server, username, .opts) {
  ## Controleer of RCurl is geinstalleerd
  check_installed_package("RCurl")
  ## Bepaal het sftp pad van de opgegeven server. De username moet in de url
  ## staan
  server_path <- paste0(
    "sftp://",
    username,
    "@",
    server
  )

  ## Bepaal de URL van het bestand
  file_url <- paste(server_path, remote_path, sep = "/")

  ## Download het bestand naar het geheugen
  bin <- RCurl::getBinaryURL(file_url,
    .opts = .opts,
    verbose = F
  )
  return(bin)
}

#' SFTP VU Download
#'
#' Functie om bestanden te downloaden van een de VU sftp server. Het serveradres
#' en de gebruikersnaam van StudentAnalytics zijn hier al ingevoerd. De functie gaat
#' er van uit dat de public en private ssh key op deze locaties staan:
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa.pub
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa
#' @param remote_path Het pad van het bestand op de server (exclusief server url "Studentanalytics.sftp.vu.nl")
#' @param local_path Optioneel kan een lokaal pad opgegeven worden waarnaar het
#' gedownloade bestand weggeschreven wordt. Als deze parameter leeggelaten wordt, wordt
#' dit bestand niet weggeschreven.
#' @param return_bin Wordt het gedownloadde bestand ook teruggegeven als binary? de
#' geretourneerde waarde is daarna in te lezen met inleesfuncties. (default = TRUE)
#' @export
sftp_vu_download <- function(remote_path, local_path, return_bin = T) {
  ## Haal de login credentials van de VU-sftp server op
  sftp_vu_login <- sftp_login_credentials_get()

  ## vervang spaties door %20.
  remote_path <- gsub(" ", "%20", remote_path, fixed = T)

  ## voer de functie sftp_download uit met de VU gegevens.
  bin <- sftp_download(
    remote_path = remote_path,
    server      = sftp_vu_login$server,
    username    = sftp_vu_login$username,
    .opts       = sftp_vu_login$.opts
  )

  ## Optioneel: Schrijf het bestand weg naar een lokaal pad.
  if (!missing(local_path)) {
    writeBin(bin, local_path)
  }
  ## Optioneel: retourneer de binary
  if (return_bin) {
    return(bin)
  }
}

################################################################################
## UPLOAD
################################################################################

#' SFTP Upload
#'
#' Functie om bestanden te uploaden naar een remote sftp server. Dit is een helper
#' functie, die gebruikt wordt in sftp_vu_upload.
#' @param local_path Het lokale pad van het bestand dat geupload moet worden.
#' @param remote_path Het pad van het bestand op de server (exclusief server url)
#' @param server het adres van de server, zonder protocol ervoor, en zonder slash op het einde. (dus exclusief "sftp://")
#' @param username De gebruikersnaam die gebruikt wordt voor authenticatie op de server
#' @param .opts Een named lijst met opties die meegegeven worden aan de ftpUpload
#' functie. De opties die voor ssh authenticatie meegegeven moeten worden zijn
#' "ssh.public.keyfile" en "ssh.private.keyfile".
sftp_upload <- function(local_path, remote_path, server, username, .opts) {
  ## Controleer of RCurl is geinstalleerd
  check_installed_package("RCurl")
  ## Bepaal het sftp pad van de opgegeven server. De username moet in de url
  ## staan
  server_path <- paste0(
    "sftp://",
    username,
    "@",
    server
  )

  ## Bepaal de URL van het bestand
  file_url <- paste(server_path, remote_path, sep = "/")

  ## Upload het bestand.
  RCurl::ftpUpload(local_path,
    file_url,
    .opts = .opts,
    verbose = F
  )
}

#' SFTP VU Upload
#'
#' Functie om bestanden te uploaden naar een de VU sftp server. Het serveradres
#' en de gebruikersnaam van StudentAnalytics zijn hier al ingevoerd. De functie gaat
#' er van uit dat de public en private ssh key op deze locaties staan:
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa.pub
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa
#' @param local_path Het lokale pad van het bestand dat geupload wordt
#' @param remote_path Het pad waarnaar het bestand op de server geupload moet worden
#' (exclusief server url "Studentanalytics.sftp.vu.nl")
#' @export
sftp_vu_upload <- function(local_path, remote_path) {
  ## Haal de login credentials van de VU-sftp server op
  sftp_vu_login <- sftp_login_credentials_get()

  ## voer de functie sftp_upload uit met de VU gegevens.
  sftp_upload(
    local_path = local_path,
    remote_path = remote_path,
    server = sftp_vu_login$server,
    username = sftp_vu_login$username,
    .opts = sftp_vu_login$.opts
  )
}



################################################################################
## UPLOAD
################################################################################

#' SFTP Delete
#'
#' Functie om bestanden te verwijderen van een remote sftp server. Dit is een helper
#' functie, die gebruikt wordt in sftp_vu_delete.
#' @param remote_path Het pad van het bestand op de server (exclusief server url)
#' @param server het adres van de server, zonder protocol ervoor, en zonder slash op het einde. (dus exclusief "sftp://")
#' @param username De gebruikersnaam die gebruikt wordt voor authenticatie op de server
#' @param .opts Een named lijst met opties die meegegeven worden aan de ftpUpload
#' functie. De opties die voor ssh authenticatie meegegeven moeten worden zijn
#' "ssh.public.keyfile" en "ssh.private.keyfile".
sftp_delete <- function(remote_path, server, username, .opts) {
  ## Controleer of RCurl is geinstalleerd
  check_installed_package("RCurl")
  ## Bepaal het sftp pad van de opgegeven server. De username moet in de url
  ## staan
  server_path <- paste0(
    "sftp://",
    username,
    "@",
    server
  )

  ## Bepaal de URL van het bestand
  file_url <- paste(server_path, remote_path, sep = "/")

  message(file_url)
  ## Upload het bestand.
  RCurl::curlPerform(
    url = server_path,
    quote = paste0("rm \"", remote_path, "\""),
    .opts = .opts,
    verbose = F
  )
}


#' SFTP VU Delete
#'
#' Functie om bestanden te verwijderen van een de VU sftp server. Het serveradres
#' en de gebruikersnaam van StudentAnalytics zijn hier al ingevoerd. De functie gaat
#' er van uit dat de public en private ssh key op deze locaties staan:
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa.pub
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa
#' @param remote_path Het pad waarnaar het bestand op de server geupload moet worden
#' (exclusief server url "Studentanalytics.sftp.vu.nl")
#' @export
sftp_vu_delete <- function(remote_path) {
  ## Haal de login credentials van de VU-sftp server op
  sftp_vu_login <- sftp_login_credentials_get()

  ## voer de functie sftp_upload uit met de VU gegevens.
  sftp_delete(
    remote_path = remote_path,
    server = sftp_vu_login$server,
    username = sftp_vu_login$username,
    .opts = sftp_vu_login$.opts
  )
}


#' SFTP VU Download and delete
#'
#' Functie om bestanden te downloaden en daarna te verwijderen van
#' de VU sftp server. Het serveradres en de gebruikersnaam van StudentAnalytics
#' zijn hier al ingevoerd. De functie gaat er van uit dat de public en private ssh
#' key op deze locaties staan:
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa.pub
#' - 99. Functies & Libraries/ssh/User keys/sa_rsa
#' @param remote_path Het pad van het bestand op de server dat gedownload moet worden
#' (exclusief server url "Studentanalytics.sftp.vu.nl")
#' @param local_folder De lokale folder waarin het bestand van de server weggeschreven
#' moet worden
#' @param overwrite FALSE is standaard. Als TRUE, dan wordt het lokale bestand overschreven
#' met het nieuwe bestand op de server.
#' @param delete TRUE is standaard. Het bestand wordt van de server verwijderd. Als FALSE wordt het
#' niet van de server verwijderd, maar wel gedownload.
#' @export
sftp_vu_download_and_delete <- function(remote_path, local_folder, overwrite = F, delete = T) {
  ## Bepaal de bestandsnaam uit het remote path
  filename <- basename(remote_path)
  ## Bepaal het lokale pad uit de lokale folder en de bestandsnaam.
  local_path <- paste0(local_folder, filename)

  ## Als overwrite FALSE is, wordt hieronder getest of het lokale bestand al
  ## bestaat.
  if (!overwrite) {
    ## Als het lokale bestand al bestaat, stopt de functie hier
    if (file.exists(local_path)) {
      stop(paste("Local file", local_path, "already exists. Therefore the file will not be downloaded from the server:", remote_path))
    }
  } else {
    ## Als het lokale bestand al bestaat, wordt een waarschuwing gegeven.
    if (file.exists(local_path)) {
      warning(paste("Local file", local_path, "already exists, will be overwritten with:", remote_path))
    }
  }

  ## Download het bestand, en schrijf het weg naar de lokale folder, onder
  ## dezelfde bestandsnaam als op de server
  sftp_vu_download(remote_path, local_path, return_bin = F)

  ## Als het bestand nu bestaat, is het succesvol gedownload. In dat geval wordt
  ## het bestand verwijderd van de server.
  if (file.exists(local_path)) {
    ## Verwijder het bestand
    if (delete) {
      ## Toon een melding
      message(paste(filename, "was succesfully downloaded from the server. The file will now be deleted from the server."))
      sftp_vu_delete(remote_path)
    } else {
      ## Toon een melding
      message("The file was not deleted from the server, as the 'delete' argument is set to FALSE")
    }
  }
}
