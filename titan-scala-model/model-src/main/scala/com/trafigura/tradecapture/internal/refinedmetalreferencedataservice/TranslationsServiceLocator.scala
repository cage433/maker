/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import org.jboss.resteasy.client._

import com.trafigura.tradecapture.internal.refinedmetal._


/**
 * Locator for finding TranslationsService instances.
 */
trait TranslationsServiceLocator {
  /**
   * Provides a proxy for the given operation to run.
   */
  def proxy:TranslationsService
}

class LocatableTranslationsService(val locator:TranslationsServiceLocator) extends TranslationsService {
  
    def getAllTranslations(): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] =
      locator.proxy.getAllTranslations()
  
    def getTranslationsForLanguage(languageCode: String): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] =
      locator.proxy.getTranslationsForLanguage(languageCode)
  
    def saveTranslations(translations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation]): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] =
      locator.proxy.saveTranslations(translations)
  
    def deleteTranslations(translations: List[Int]): Boolean =
      locator.proxy.deleteTranslations(translations)
  
    def getAllLanguages(): List[com.trafigura.tradecapture.internal.refinedmetal.Language] =
      locator.proxy.getAllLanguages()
  
    def getTranslations(languageCode: String, regTranslations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation]): com.trafigura.tradecapture.internal.refinedmetal.Translations =
      locator.proxy.getTranslations(languageCode, regTranslations)
  
}