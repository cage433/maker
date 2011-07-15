// <copyright>
//   Copyright (c) Microsoft Corporation.  All rights reserved.
// </copyright>

// This file is used by Code Analysis to maintain SuppressMessage 
// attributes that are applied to this project.
// Project-level suppressions either have no target or are given 
// a specific target and scoped to a namespace, type, member, etc.
//
// To add a suppression to this file, right-click the message in the 
// Error List, point to "Suppress Message(s)", and click 
// "In Project Suppression File".
// You do not need to add suppressions to this file manually.

[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1001:TypesThatOwnDisposableFieldsShouldBeDisposable", Scope = "type", Target = "ContactManager.ContactRepository")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope", Scope = "member", Target = "ContactManager.PngFormatter.#WriteToStream(System.Object,System.IO.Stream)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA2210:AssembliesShouldHaveValidStrongNames")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "0", Scope = "member", Target = "ContactManager.ContactRepository.#Post(ContactManager.Contact)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "0", Scope = "member", Target = "ContactManager.ContactRepository.#Update(ContactManager.Contact)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.PngFormatter.#WriteToStream(System.Object,System.IO.Stream,Microsoft.Http.HttpRequestMessage)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Png", Scope = "type", Target = "ContactManager.PngFormatter")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1716:IdentifiersShouldNotMatchKeywords", MessageId = "Get", Scope = "member", Target = "ContactManager.IContactRepository.#Get(System.Int32)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Scope = "member", Target = "ContactManager.LocationProcessor.#OnExecute(System.String,System.String)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "1#", Scope = "member", Target = "ContactManager.LocationProcessor.#OnExecute(System.String,System.String)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline", Scope = "member", Target = "ContactManager.ContactRepository.#.cctor()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1822:MarkMembersAsStatic", Scope = "member", Target = "ContactManager.ContactsResource.#GetLocation(ContactManager.Location)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope = "member", Target = "ContactManager.ContactRepository.#GetAll()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope = "member", Target = "ContactManager.ContactsResource.#GetAll()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope = "member", Target = "ContactManager.IContactRepository.#GetAll()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate", Scope = "member", Target = "ContactManager.ContactsResource.#GetAll()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate", Scope = "member", Target = "ContactManager.IContactRepository.#GetAll()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "0", Scope = "member", Target = "ContactManager.ContactManagerConfiguration.#RegisterRequestProcessorsForOperation(System.ServiceModel.Description.HttpOperationDescription,System.Collections.Generic.IList`1<System.ServiceModel.Dispatcher.Processor>,Microsoft.ServiceModel.Http.MediaTypeProcessorMode)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.ContactManagerConfiguration.#RegisterRequestProcessorsForOperation(System.ServiceModel.Description.HttpOperationDescription,System.Collections.Generic.IList`1<System.ServiceModel.Dispatcher.Processor>,Microsoft.ServiceModel.Http.MediaTypeProcessorMode)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "0", Scope = "member", Target = "ContactManager.ContactManagerConfiguration.#RegisterResponseProcessorsForOperation(System.ServiceModel.Description.HttpOperationDescription,System.Collections.Generic.IList`1<System.ServiceModel.Dispatcher.Processor>,Microsoft.ServiceModel.Http.MediaTypeProcessorMode)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.ContactManagerConfiguration.#RegisterResponseProcessorsForOperation(System.ServiceModel.Description.HttpOperationDescription,System.Collections.Generic.IList`1<System.ServiceModel.Dispatcher.Processor>,Microsoft.ServiceModel.Http.MediaTypeProcessorMode)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists", Scope = "member", Target = "ContactManager.ContactsResource.#Get()")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.ContactResource.#Get(System.String,Microsoft.Http.HttpResponseMessage)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.ContactsResource.#Post(ContactManager.Contact,Microsoft.Http.HttpResponseMessage)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "1", Scope = "member", Target = "ContactManager.PngProcessor.#WriteToStream(System.Object,System.IO.Stream,Microsoft.Http.HttpRequestMessage)")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Png", Scope = "type", Target = "ContactManager.PngProcessor")]
